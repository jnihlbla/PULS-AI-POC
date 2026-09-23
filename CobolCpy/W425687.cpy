000100 01  W425687.                                                             
000200*                                 FAKTURARAD TILL VR-SYSTEM               
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 KDFAKTYP             PIC X.                                       
001500*                                 FAKTURATYP                              
001600     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001700*                                 PRODUKTIONSNUMMER                       
001800     03 KDORDKL              PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000     03 IDKUNDRF-RO          PIC X(10).                                   
002100*                                 KUND REF PÅ RO                          
002200     03 IDARTNR              PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002500*                                 KOLLINUMMER                             
002600     03 KVLEVART             PIC S9(7)           COMP-3.                  
002700*                                 LEVERERAT ANTAL STYCK                   
002800     03 KDVRINFO             PIC S9              COMP-3.                  
002900*                                 PÅVERKAN I VR/DSP SYSTEM                
003000     03 KDSRA                PIC S9(3)           COMP-3.                  
003100*                                 SRA-KOD                                 
003200     03 KDARTURS             PIC X(2).                                    
003300*                                 ARTIKELURSPRUNGSKOD                     
003400     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
003500*                                 FUNKTIONSGRUPP                          
003600     03 KDPRODSL             PIC S9(3)           COMP-3.                  
003700*                                 PRODUKTSLAG                             
003800     03 KDVVKL               PIC S9              COMP-3.                  
003900*                                 VOLYMVÄRDESKLASS                        
004000     03 KVQPACK              PIC S9(5)           COMP-3.                  
004100*                                 ANTAL KVANTITETFÖRPACKNINGAR            
004200     03 VKART                PIC S9(7)           COMP-3.                  
004300*                                 ARTIKELVIKT (G)                         
004400     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
004500*                                 ARTIKELPRIS NETTO                       
004600     03 PRARTULL             PIC S9(7)V9(2)      COMP-3.                  
004700*                                 TULLPRIS PER ARTIKEL                    
004800     03 KDRABATT             PIC S9(3)           COMP-3.                  
004900     03 BEART                PIC X(25).                                   
005000*                                 ARTIKELBENÄMNING                        
005100     03 KDSORT               PIC X(2).                                    
005200*                                 SORT-KOD                                
005300     03 FILLER               PIC X(4).                                    
005400     03 IDKUNDRF             PIC X(10).                                   
005500*                                 KUNDENS REFERENS (ORDERID)              
005600     03 TIPRIS               PIC S9(7)           COMP-3.                  
005700*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
005800     03 KDVIP                PIC X.                                       
005900*                                 VIP-KOD                                 
006000     03 BERADREF             PIC X(10).                                   
006100*                                 KUNDENS RADREFERENS                     
006200     03 KDTULLVE             PIC S9              COMP-3.                  
006300*                                 TYP AV PRIS PÅ TULLFAKTURA              
006400     03 REBPRIS              PIC S9(4)V9(1)      COMP-3.                  
006500*                                 BASPRISNIVÅ                             
006600     03 FLPRTILL             PIC X.                                       
006700*                                 PRISTILLÄGGS FLAGGA                     
006800     03 PRARTBTO-EXP         PIC S9(7)V9(2)      COMP-3.                  
006900*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
007000     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
007100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
007200     03 TIKLOCK              PIC S9(9)           COMP-3.                  
007300*                                 KLOCKSLAG (TTMMSSTH)                    
007400     03 IDKLIENT             PIC X(10).                                   
007500*                                 VADIS KLIENT                            
007600     03 IDARBREF             PIC X(10).                                   
007700*                                 ARBETSORDER VADIS                       
007800     03 IDBIL.                                                            
007900*                                 BILIDENTITET                            
008000        05 IDBILTYP          PIC X(3).                                    
008100*                                 BILTYP                                  
008200        05 TIAAAA            PIC X(4).                                    
008300*                                 ÅRTAL (ÅÅÅÅ)                            
008400        05 IDCHASSI-PIE      PIC X(6).                                    
008500*                                 CHASSINUMMER PIE                        
008600     03 IDVIN                PIC X(17).                                   
008700*                                 VIN ID FORDON                           
008800*** END OF VILMAII-COPY LENGTH= 199 BYTES                                 
