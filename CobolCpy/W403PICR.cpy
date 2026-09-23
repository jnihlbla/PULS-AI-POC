000100 01  PICR-W403PICR.                                                       
000200*                                 3IV PICK TASK RESULT POST               
000300*                                 3IV PICK TASK RESULT RECORD             
000400*                                 IDRTYP3IV="PickTaskResult"              
000500     03 PICR-IDRTYP3IV       PIC X(30).                                   
000600*                                 3IV RECORD-TYP                          
000700*                                 3IV RECORD TYPE                         
000800     03 PICR-IDPURAD         PIC 9(4).                                    
000900*                                 RADNUMMER PÅ PACKUNDERLAG               
001000*                                 LINENO IN PACKINGDOCUMENT               
001100     03 PICR-KVAVBART        PIC 9(6).                                    
001200*                                 AVBOKAT ANTAL ARTIKLAR                  
001300*                                 ALLOCATED QUANTITY                      
001400     03 PICR-KVLEVART        PIC 9(6).                                    
001500*                                 LEVERERAT ANTAL STYCK                   
001600*                                 DELIVERED QUANTITY                      
001700     03 PICR-KDARTURS-NUM    PIC 9(2).                                    
001800*                                 ARTIKELURSPRUNGSKOD NUMERISK            
001900*                                 COUNTRY OF ORIGIN NUMERIC               
002000*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
