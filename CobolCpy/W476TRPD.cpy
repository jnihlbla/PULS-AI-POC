000100 01  TRPD-W476TRPD.                                                       
000200*                                 PARAMETER TO SUBPROGRAMS FOR            
000300*                                 PRINTING TRANSPORT DOCUMENT             
000400     03 TRPD-IDSHIPM         PIC 9(7).                                    
000500*                                 SKEPPNINGSNUMMER                        
000600*                                 SHIPMENT NO                             
000700     03 TRPD-IDPRTLST        PIC X(8).                                    
000800*                                 LOGISK PRINTER+LISTA IDENTITET          
000900*                                 LOGICAL PRINTER+LIST IDENTITY           
001000     03 TRPD-PFDEF-OVR       PIC X(8).                                    
001100     03 TRPD-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 TRPD-KVCOPIES        PIC X.                                       
001500*                                 ANTAL COPIOR VID PRINTNING              
001600*                                 NUMBER OF PRINTED COPIES                
001700     03 TRPD-IDPGM           PIC X(8).                                    
001800*                                 PROGRAM IDENTITET                       
001900*                                 PROGRAM INTENTITY                       
002000     03 TRPD-FLSKRIV-ONDEM   PIC X.                                       
002100*                                 J/Y = SKRIV BEGÄRD LISTA                
002200*                                 J/Y = PRINT REPORT NOW                  
002300     03 TRPD-FLLDCKND        PIC X.                                       
002400*                                 FL LDC-KUND                             
002500*                                 FL LDC CUSTOMER                         
002600*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
