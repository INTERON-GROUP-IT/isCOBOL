       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. dcicolumns.                                  
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL. 
           select customer
              assign       to  "customer001"
              organization is indexed
              access mode  is dynamic
              lock mode    is manual
              file status  is cust-status
              record key   is cust-code
              alternate record key is cust-name with duplicates.
                                            
       DATA DIVISION.                                                   
       FILE SECTION. 
       fd customer.
       01  Customer-Rec.
           05 Cust-Code           PIC 9(5).
           05 Cust-Name.
              10 Cust-First-Name  PIC X(30).
              10 Cust-Last-Name PIC X(30).
           05 Cust-Address.
               10 Cust-Street     PIC X(30).
               10 Cust-City       PIC X(20).
               10 Cust-State      PIC X(20).
               10 Cust-Zip        PIC X(5).
           05 Cust-Gender         PIC X.
           05 Cust-Phone          PIC X(15).
           05 Cust-CellPhone      PIC X(15).


       WORKING-STORAGE SECTION.     
        01 cust-status                 PIC X(02) VALUE SPACES.       	
          88 VALID-cust                VALUE "00" THRU "09". 
		01 wcomando                    PIC X any length.	  
        01 is-file pic x any length.	
        01 w-msg pic x any LENGTH.		
        01  crerr-status.
           03 file-status          pic xx.
           03 ext-status           pic x(10).
       PROCEDURE DIVISION.
	 inicio.
           display window erase. 
          
           SET ENVIRONMENT "file.index" to "dci"
	     SET ENVIRONMENT "io_creates" to "1"
		   
	     perform seta-colunas
	     OPEN input CUSTOMER
                             
           if not valid-cust 
              perform valida-status
              GOBACK
           end-if. 
		   
	     display "colunas chaves + colunas desejadas"
	     display "Abertura INPUT"
	     display "Retornar apenas a coluna Cust-Phone" 
		   
           
           read customer next
           display Customer-Rec
                      
           accept omitted
           close customer
           goback.    
                      
           
       seta-colunas.  
       
           string "customer001=" delimited by size 
                  "cust_code,cust_first_name,cust_last_name,cust_phone"  
                  delimited by size into wcomando
	       display wcomando			  
           call "DCI_SETENV" using "DCI_COLUMNS_MAPPING" wcomando.   
           
       valida-status.
           call "c$rerrname" using is-file
           call "C$RERR"  using crerr-status  w-msg.
           display message box "File : " is-file x"0a"
                               "Status : " file-status x"0a"
                               "Extendido : " ext-status x"0a"
                               w-msg.                
            
      