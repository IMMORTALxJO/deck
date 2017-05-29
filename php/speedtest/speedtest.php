<?php
	if ( isset($_GET[ 'total' ]) ){
		require_once( './index.php' );
		die();
	}
	ini_set( 'display_errors', 0 );
	ini_set( 'open_basedir', '' );

	class TEST {
		private $ROOT = '/';
		private $timers = [];
		private $answer = [];
		private $config = [
			'mysql'=> [
				'host' => false,
				'database' => false,
				'user' => false,
				'pass' => false,
				'count' => 1000
			],
			'cpu' => [
				'count' => 5000000
			],
			'fs' => [
				'count' => 1000
			],
			'total' => [
				'target_url'=>false,
				'count' => 5
			]
		];

		private function start_timer( $name ){
			$this->timers[ $name ] = microtime(true)*1000;
			$this->answer[ $name ] = 0;
		}

		private function stop_timer( $name, $passed = true ){
			$this->timers[ $name ] =  microtime(true)*1000 - $this->timers[ $name ];
			if ( !$passed ){
				$this->answer[ $name ] = 0;
				return;
			}
			$this->answer[ $name ] = floor( $this->config[ $name ][ 'count' ] * ( 1000 / $this->timers[ $name ] ) );
		}

		private function mysql_find_config(){
			$ROOT = $this->ROOT;
			$CMS = 'unknown';
			if( file_exists( $ROOT.'/wp-config.php' ) ) $CMS = 'WP';
			else if( file_exists( $ROOT.'/bitrix/php_interface/dbconn.php' ) ) $CMS = 'bitrix';
			else if( file_exists( $ROOT.'/configuration.php' ) ) $CMS = 'joomla';

			switch ( $CMS ) {
				case 'WP':
					require_once( $ROOT.'/wp-config.php' );
					$this->config[ 'mysql' ][ 'host' ] = explode( ':', DB_HOST)[0];
					$this->config[ 'mysql' ][ 'database' ] = DB_NAME;
					$this->config[ 'mysql' ][ 'user' ] = DB_USER;
					$this->config[ 'mysql' ][ 'pass' ] = DB_PASSWORD;
				break;
				case 'bitrix':
					require_once( $ROOT.'/bitrix/php_interface/dbconn.php' );
					$this->config[ 'mysql' ][ 'host' ] = $DBHost;
					$this->config[ 'mysql' ][ 'database' ] = $DBName;
					$this->config[ 'mysql' ][ 'user' ] = $DBLogin;
					$this->config[ 'mysql' ][ 'pass' ] = $DBPassword;
					unset($DBHost);
					unset($DBName);
					unset($DBLogin);
					unset($DBPassword);
				break;
				case 'joomla':
					require_once( $ROOT.'/configuration.php' );
					$JConfig = new JConfig();
					$this->config[ 'mysql' ][ 'host' ] = $JConfig->host;
					$this->config[ 'mysql' ][ 'database' ] = $JConfig->db;
					$this->config[ 'mysql' ][ 'user' ] = $JConfig->user;
					$this->config[ 'mysql' ][ 'pass' ] = $JConfig->password;
					unset($JConfig);
				break;
			}
		}

		public function init(){
			$this->ROOT = realpath( dirname(__FILE__) );
			if( !$this->config[ 'mysql' ][ 'host' ] )
				$this->mysql_find_config();
			$this->mysql();
			$this->cpu();
			$this->fs();
			$this->total();
		}

		public function end(){
			$msg = '';
			$this->answer[ 'cpu' ] = round( $this->answer[ 'cpu' ]/1000000, 3 );
			list( $this->answer[ 'la1' ], $this->answer[ 'la5' ], $this->answer[ 'la15' ] ) = sys_getloadavg();
			foreach ($this->answer as $key => $value)
				$msg .= '"'.$key.'":'.round( $value, 3 ).',';
			die( '{'.$msg.'"hostname":"'.gethostname().'"}' );
		}

		public function mysql(){
			$table_name = bin2hex( random_bytes( 8 ) );
			$count = $this->config[ 'mysql' ][ 'count' ];
			$good = true;
			$this->start_timer( 'mysql' );
			$mysqli = new mysqli(
				$this->config[ 'mysql' ]['host'],
				$this->config[ 'mysql' ]['user'],
				$this->config[ 'mysql' ]['pass'],
				$this->config[ 'mysql' ]['database']
			);
			if( !$mysqli )
				$good = false;
			if( $good && !$mysqli->query( "CREATE TABLE `{$table_name}` ( `id` INT NOT NULL , `string` VARCHAR(100) , PRIMARY KEY (`id`)) DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci;" ) )
				$good = false;

			while( $good && $count-- )
				$mysqli->query( "INSERT INTO `{$table_name}` (`id`, `string`) VALUES ({$count}, '{$count}_{$count}_{$count}');" );
			if( $good && !$mysqli->query("DROP TABLE `{$table_name}`;") )
				$good = false;
			if( $good && !$mysqli->close() )
				$good = false;

			$this->stop_timer( 'mysql', $good );
		}

		public function cpu(){
			$count = $this->config[ 'cpu' ][ 'count' ];
			$this->start_timer( 'cpu' );
			while( $count ){
				$count++;$count--;
				$count++;$count--;
				$count++;$count--;
				$count--;
			};
			$this->stop_timer( 'cpu' );
		}

		public function fs(){
			$ROOT = $this->ROOT;
			$dir_name = $ROOT."/".bin2hex( random_bytes( 8 ) );
			$count = $this->config[ 'fs' ][ 'count' ];
			$sum = 0;
			$this->start_timer('fs');
			mkdir( $dir_name, 0755 );
			for( $i=0; $i<$count; $i++ ){
				$sum += $i;
				file_put_contents( "{$dir_name}/{$i}.php", "<?php \$sum-={$i};;?>" );
			}
			for( $i=0; $i<$count; $i++ ){
				require_once( "{$dir_name}/{$i}.php" );
				unlink( "{$dir_name}/{$i}.php" );
			}
			rmdir($dir_name);
			( $sum === 0 )?
				$this->stop_timer( 'fs' ):
				$this->stop_timer( 'fs', false);

		}

		public function total(){
			$target_url = ( $this->config[ 'total' ][ 'target_url' ] )? $this->config[ 'total' ][ 'target_url' ] : "http://{$_SERVER['SERVER_NAME']}{$_SERVER[ 'REQUEST_URI' ]}?total" ;
			$count = $this->config[ 'total' ][ 'count' ]+1;
			$total_time = 0;
			for( $i=0; $i<$count; $i++ ){
				$start_time = microtime(true)*1000;
				if( file_get_contents( $target_url ) ){
					if($i>0) $total_time += microtime(true)*1000 - $start_time;
				}else{
					$this->stop_timer( 'total', false);
					return;
				}
			}
			$total_time /= $count-1;
			$this->answer[ 'total' ] = round( 250/$total_time, 3 );
		}
	}

	$TEST = new TEST();
	$TEST->init();
	$TEST->end();
?>

