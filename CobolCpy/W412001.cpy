000010 01  W412001.                                                             
000020*                                 ID-DEL FÖR PT 001                       
000030*                                 POSTER FRÅN WDQ2 SOM HAR                
000040*                                 KLASS = 0 ELLER FÖRBIORDER              
000050     03 IDPTYP               PIC X(3).                                    
000060*                                 POSTTYP                                 
000070     03 IDORDER              PIC S9(7)           COMP-3.                  
000080*                                 VOLVO PARTS ORDERNUMMER                 
000090     03 IDDISTR              PIC S9(5)           COMP-3.                  
000100*                                 DISTRIKTNUMMER                          
000110     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000120*                                 KUNDNUMMER                              
000130     03 IDKUNDRF             PIC X(10).                                   
000140*                                 KUNDENS REFERENS (ORDERID)              
000150     03 KDORDKL              PIC S9              COMP-3.                  
000160*                                 ORDERKLASS                              
000170     03 FLFORBI              PIC X.                                       
000180*                                 FÖRBIORDERFLAGGA                        
000190     03 IDUSER               PIC X(8).                                    
000200*                                 ANVÄNDARENS SÄKERHETS ID                
      *** END COPY W412001     LENGTH=34                                        
