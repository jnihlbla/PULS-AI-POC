000010 01  W37120.                                                              
000020*                                 INFO TILL UPPDATERING AV WDA8           
000030*                                                                         
000040     03 IDPTYP               PIC X(3).                                    
000050*                                 POSTTYP                                 
000060     03 IDDISTR              PIC S9(5)           COMP-3.                  
000070*                                 DISTRIKTNUMMER                          
000080     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000090*                                 KUNDNUMMER                              
000100     03 IDARTNR              PIC S9(9)           COMP-3.                  
000110*                                 ARTIKELNUMMER                           
000120     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
000130*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
000140     03 IDLAGER              PIC X(2).                                    
000150*                                 IDENTIFIERARE LAGER                     
000160     03 IDORDNR              PIC S9(5)           COMP-3.                  
000170*                                 ORDERNUMMER                             
000180     03 KVANTAL              PIC S9(7)           COMP-3.                  
000190*                                 ANTAL ALLMÄNT                           
000200     03 FLINVEST             PIC X.                                       
000210*                                 BYTES INVENTERINGSFLAGGA                
000220     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
000230*                                 OBJEKTNUMMER                            
000240     03 KDANMORS             PIC S9(3)           COMP-3.                  
000250*                                 ORSAK TILL LEVERANSANMÄRKNING           
000260     03 IDKUNDRF             PIC X(10).                                   
000270*                                 KUNDENS REFERENS (ORDERID)              
      *** END COPY W37120      LENGTH=46                                        
