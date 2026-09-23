000100 01  W475F805.                                                            
000200*                                 FAKTURARAD TILL VR-SYSTEM               
000300*                                 POSTTYP VR5                             
000400     03 IDKUNDRF-RO          PIC X(10).                                   
000500*                                 KUND REF PÅ RO                          
000600     03 KVLEVART             PIC S9(7)           COMP-3.                  
000700*                                 LEVERERAT ANTAL STYCK                   
000800     03 KDVRINFO             PIC S9              COMP-3.                  
000900*                                 PÅVERKAN I VR/DSP SYSTEM                
001000     03 KDSRA                PIC S9(3)           COMP-3.                  
001100*                                 SRA-KOD                                 
001200     03 KDARTURS             PIC X(2).                                    
001300*                                 ARTIKELURSPRUNGSKOD                     
001400     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001500*                                 FUNKTIONSGRUPP                          
001600     03 KVQPACK              PIC S9(5)           COMP-3.                  
001700*                                 ANTAL KVANTITETFÖRPACKNINGAR            
001800     03 VKART                PIC S9(7)           COMP-3.                  
001900*                                 ARTIKELVIKT (G)                         
002000     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
002100*                                 ARTIKELPRIS NETTO                       
002200     03 PRARTULL             PIC S9(7)V9(2)      COMP-3.                  
002300*                                 TULLPRIS PER ARTIKEL                    
002400     03 KDRABATT             PIC S9(3)           COMP-3.                  
002500     03 BEART                PIC X(25).                                   
002600*                                 ARTIKELBENÄMNING                        
002700     03 TIPRIS               PIC S9(7)           COMP-3.                  
002800*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
002900     03 BERADREF             PIC X(10).                                   
003000*                                 KUNDENS RADREFERENS                     
003100     03 KDTULLVE             PIC S9              COMP-3.                  
003200*                                 TYP AV PRIS PÅ TULLFAKTURA              
003300     03 REBPRIS              PIC S9(4)V9(1)      COMP-3.                  
003400*                                 BASPRISNIVÅ                             
003500     03 FLPRTILL             PIC X.                                       
003600*                                 PRISTILLÄGGS FLAGGA                     
003700     03 PRARTBTO-EXP         PIC S9(7)V9(2)      COMP-3.                  
003800*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
003900     03 IDKLIENT             PIC X(10).                                   
004000*                                 VADIS KLIENT                            
004100     03 IDARBREF             PIC X(10).                                   
004200*                                 ARBETSORDER VADIS                       
004300     03 IDBIL.                                                            
004400*                                 BILIDENTITET                            
004500        05 IDBILTYP          PIC X(3).                                    
004600*                                 BILTYP                                  
004700        05 TIAAAA            PIC X(4).                                    
004800*                                 ÅRTAL (ÅÅÅÅ)                            
004900        05 IDCHASSI-PIE      PIC X(6).                                    
005000*                                 CHASSINUMMER PIE                        
005100     03 IDVIN                PIC X(17).                                   
005200*                                 VIN ID FORDON                           
005300     03 KDSORT               PIC X(2).                                    
005400*                                 SORT-KOD                                
005500*** END OF VILMAII-COPY LENGTH= 142 BYTES                                 
