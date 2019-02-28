FROM ubuntu:latest
COPY ./clean_all_except.sh /clean.sh
RUN /clean.sh --dir /usr --dir /bin --dir /lib --dir /lib64
RUN $( /clean.sh --dir /usr --dir /bin --dir /lib --dir /lib64 )