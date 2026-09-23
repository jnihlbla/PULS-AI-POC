000100 01  W601-W6010011.                                                       
000200*                                 COPYTEXT TILL UPPFÖLJNINGSTRANS         
000300*                                 AR                                      
000400*                                 FRÅN INLEVERANSSYSTEMET                 
000500*                                                                         
000600     03 W601-IDLOPNRM        PIC S9(9)           COMP-3.                  
000700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000800*                                 (0VVDLLLLK)                             
000900     03 W601-IDRADNR         PIC S9(5)           COMP-3.                  
001000*                                 RADNUMMER                               
001100     03 W601-KDINLPRIO       PIC S9(3)           COMP-3.                  
001200*                                 PRIORITETSGRUPP                         
001300     03 W601-KDINLSTA        PIC X(3).                                    
001400*                                 SYSTEMSTATUS INLEVERANS                 
001500     03 W601-KDINLUPF        PIC X(4).                                    
001600*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
001700     03 W601-KDINLUPF-NXT    PIC X(4).                                    
001800*                                 NÄSTA UPPFÖLJNINGSSTATUS INLEVE         
001900*                                 RANS                                    
002000     03 W601-KVINLART        PIC S9(7)           COMP-3.                  
002100*                                 ANTAL I PARTIRAD                        
002200     03 W601-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
002300*                                 ARTIKELSTANDARDPRIS                     
002400     03 W601-KVKOLLI         PIC S9(5)           COMP-3.                  
002500*                                 ANTAL KOLLI                             
002600     03 W601-FLINLI          PIC X.                                       
002700*                                 INLAGD RAD, PARTI ELLER KOLLI           
002800*** END COPY W6010011    LENGTH=34                                        
