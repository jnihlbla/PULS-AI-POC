000100 01  REQU-W50293I1.                                                       
000200*                                 REQUEST-COPYTEXT FÖR BILD 5293          
000300*                                 VID ANROP FRÅN WEBBEN                   
000400*                                 ACS FOLLOW-UP SELECTION                 
000500     03 REQU-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 REQU-KDCMDVAL        PIC X.                                       
000900*                                 GENERELL KOMMANDOKOD                    
001000*                                 GENERAL COMMAND-CODE                    
001100     03 REQU-ADLAGOMR        PIC 9(2).                                    
001200*                                 LAGEROMRÅDE                             
001300*                                 AREA                                    
001400     03 REQU-ADGANG-FOM      PIC 9(2).                                    
001500*                                 GÅNG FRÅN OCH MED                       
001600*                                 AISLE ADDRESS FROM                      
001700     03 REQU-ADGANG-TOM      PIC 9(2).                                    
001800*                                 GÅNG TILL OCH MED                       
001900*                                 AISLE ADDRESS TO                        
002000     03 REQU-IDPRTOMG        PIC 9.                                       
002100*                                 PRINTOMGÅNG FÖR INVENT/JUSTERIN         
002200*                                 PRINT ROUND OF INVENTORY/ADJUST         
002300*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  
