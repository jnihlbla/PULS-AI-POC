000100 01  W601-W6010012.                                                       
000200*                                 UPPFÖLJNINGSTRANS                       
000300*                                 FRÅN INLEVERANSSYSTEMET                 
000400*                                 /                                       
000500*                                 FOLLOW-UP TRANSACTION                   
000600*                                 FROM THE GOODS RECEIVING SYSTEM         
000700     03 W601-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 W601-IDLOPNRM        PIC S9(9)           COMP-3.                  
001100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001200*                                 (0VVDLLLLK)                             
001300*                                 SERIAL NO RECEIVING REPORT              
001400*                                 (0WWDLLLLC)                             
001500     03 W601-IDRADNR         PIC S9(5)           COMP-3.                  
001600*                                 RADNUMMER                               
001700*                                 LINE NO                                 
001800     03 W601-KDINLPRIO       PIC S9(3)           COMP-3.                  
001900*                                 PRIORITETSGRUPP                         
002000*                                 PRIORITY GROUP                          
002100     03 W601-KDINLSTA        PIC X(3).                                    
002200*                                 SYSTEMSTATUS INLEVERANS                 
002300*                                 SYSTEM STATUS RECEIVING                 
002400     03 W601-KDINLUPF        PIC X(4).                                    
002500*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
002600*                                 FOLLOW-UP STATUS RECEIVING              
002700     03 W601-KDINLUPF-NXT    PIC X(4).                                    
002800*                                 NÄSTA UPPFÖLJNINGSSTATUS INLEVE         
002900*                                 RANS                                    
003000*                                 NEXT FOLLOW-UP STATUS RECEIVING         
003100     03 W601-KVINLART        PIC S9(7)           COMP-3.                  
003200*                                 ANTAL I PARTIRAD                        
003300*                                 QTY/LINE IN A LOT                       
003400     03 W601-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
003500*                                 ARTIKELSTANDARDPRIS                     
003600*                                 STANDARD PRICE                          
003700     03 W601-KVKOLLI         PIC S9(5)           COMP-3.                  
003800*                                 ANTAL KOLLI                             
003900*                                 NBR OF CASES                            
004000     03 W601-FLINLI          PIC X.                                       
004100*                                 INLAGD RAD, PARTI ELLER KOLLI           
004200*                                 STORED  LINE                            
