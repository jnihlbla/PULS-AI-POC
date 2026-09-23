000100 01  RAPPORT-FAKT-DENNA-V.                                                
000200*                                 RAPPORT-POST FÖR BYTES                  
000300*                                 FAKTURERAT DENNA VECKA                  
000400*                                                                         
000500     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
000600*                                 OBJEKTNUMMER                            
000700     03 REKSIFFR             PIC S9              COMP-3.                  
000800*                                 KONTROLLSIFFRA                          
000900     03 BEART-OBJ            PIC X(25).                                   
001000*                                 ARTIKELBENÄMNING                        
001100     03 IDORDNR              PIC S9(5)           COMP-3.                  
001200*                                 ORDERNUMMER                             
001300     03 KVRETUR-REST         PIC S9(7)           COMP-3.                  
001400*                                 ANTAL I RETUR                           
001500     03 SUPRIS               PIC S9(7)V9(2)      COMP-3.                  
001600*                                 RAD TOTAL                               
001700     03 IDFAKT               PIC X(9).                                    
001800*                                 FAKTURAIDENTITET     IDFAKT-003         
001900*** END COPY W3716107C0  LENGTH=52                                        
