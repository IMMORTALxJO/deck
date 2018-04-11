## TEMPLINATOR

Tool for easy template updating. It can update dict of state and commit it to some template.

Add data to state file:
```
$ templinator.py --key DEV --state-file ./project_deploy.state --User 'Mr Cucumber' --Date "$(date +'%F %T')" --Hash e43r1a89
$ templinator.py --key QA --state-file ./project_deploy.state --User 'IMMORTALxJO' --Date "$(date +'%F %T')" --Hash 0de86f2e --AnyKey someValue

$ cat ./project_deploy.state 
{
  "DEV": {
    "Date": "2018-04-11 17:16:08",
    "Hash": "e43r1a89",
    "User": "Mr Cucumber"
  },
  "QA": {
    "Date": "2018-04-11 17:17:23",
    "Hash": "0de86f2e",
    "User": "IMMORTALxJO",
    "AnyKey": "someValue"
  }
}
```

Make awesome template:
```
$ cat ./template.j2
<table>
{% for key in state %}
  <tr>
   <td>{{ key }}</td>
   <td>{{ state[key].User }}</td>
   <td>{{ state[key].Date }}</td>
   <td>{{ state[key].Hash }}</td>
  </tr>
{% endfor %}
</table>
```

Commit state to template ( render prints to stdout ):
```
$ templinator.py --state-file ./project_deploy.state --template ./template.j2
<table>

  <tr>
   <td>DEV</td>
   <td>Mr Cucumber</td>
   <td>2018-04-11 17:16:08</td>
   <td>e43r1a89</td>
  </tr>

  <tr>
   <td>QA</td>
   <td>IMMORTALxJO</td>
   <td>2018-04-11 17:17:23</td>
   <td>0de86f2e</td>
  </tr>

</table>
```

## Pro tips

If you want to look like a real pro you can update and commit changes in one step:
```
$ templinator.py --key DEV --state-file ./project_deploy.state --User 'Mr Cucumber' --Date "$(date +'%F %T')" --Hash e43r1a89 --template ./template.j2 
<table>

  <tr>
   <td>DEV</td>
   <td>Mr Cucumber</td>
   <td>2018-04-11 21:43:49</td>
   <td>e43r1a89</td>
  </tr>

</table>
$ templinator.py --key QA --state-file ./project_deploy.state --User 'IMMORTALxJO' --Date "$(date +'%F %T')" --Hash 0de86f2e --template ./template.j2 
<table>

  <tr>
   <td>DEV</td>
   <td>Mr Cucumber</td>
   <td>2018-04-11 21:43:49</td>
   <td>e43r1a89</td>
  </tr>

  <tr>
   <td>QA</td>
   <td>IMMORTALxJO</td>
   <td>2018-04-11 21:44:19</td>
   <td>0de86f2e</td>
  </tr>

</table>

```