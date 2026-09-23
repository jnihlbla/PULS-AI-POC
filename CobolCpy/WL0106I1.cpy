000100 01  REQU-WL0106I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0106             
000300*                                 LDC LOCATION INQUIRY                    
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-KDCMDVAL        PIC X(3).                                    
000800*                                 GENERELL KOMMANDOKOD                    
000900*                                 GENERAL COMMAND-CODE                    
001000     03 REQU-ADLAGOMR-KEY    PIC 9(3).                                    
001100*                                 LAGEROMRÅDE                             
001200*                                 AREA                                    
001300     03 REQU-ADGANG-KEY      PIC 9(3).                                    
001400*                                 GÅNG                                    
001500*                                 AISLE                                   
001600     03 REQU-ADPLATS-KEY     PIC 9(5).                                    
001700*                                 LAGERPLATSNUMMER                        
001800*                                 LOCATION                                
001900*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
