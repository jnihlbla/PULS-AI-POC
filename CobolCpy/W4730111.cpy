000100 01  W4730111-CTX.                                                        
000200*                                 PACKNING KOLLI TILL SVENSKA ÅF          
000300*                                 POSTTYP   111                           
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDKOLLI              PIC S9(5)           COMP-3.                  
000900*                                 KOLLINUMMER                             
001000     03 IDORDNR7             PIC S9(7)           COMP-3.                  
001100*                                 ORDERNUMMER                             
001200     03 KVLEVART             PIC S9(7)           COMP-3.                  
001300*                                 LEVERERAT ANTAL STYCK                   
001400     03 KDORDKL              PIC S9              COMP-3.                  
001500      88 KDORDKL-VOR         VALUE +0.                                    
001600      88 KDORDKL-DAG         VALUE +1.                                    
001700      88 KDORDKL-2           VALUE +2.                                    
001800      88 KDORDKL-SNABB       VALUE +2.                                    
001900      88 KDORDKL-SPECIAL     VALUE +3.                                    
002000      88 KDORDKL-KVANT       VALUE +4.                                    
002100      88 KDORDKL-SATS        VALUE +5.                                    
002200*                                 ORDERKLASS                              
002300     03 KDRO                 PIC S9              COMP-3.                  
002400*                                 RESTORDERKOD PÅ INFORMATION             
002500*                                 TILL VR                                 
002600     03 BERADREF             PIC X(10).                                   
002700*                                 KUNDENS RADREFERENS                     
002800     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
002900*                                 ARTIKELPRIS NETTO                       
003000*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
