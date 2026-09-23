000100 01  W6124A.                                                              
000200*                                 LDC REFILL AVVIKELSE VID INLEVE         
000300*                                 RANS                                    
000400     03 DAFAKT               PIC 9(8).                                    
000500*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDDC-REC             PIC X(2).                                    
000900*                                 MOTTAGANDE LAGER                        
001000     03 IDDC-SEND            PIC X(2).                                    
001100*                                 SÄNDANDE LAGER                          
001200     03 IDFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 IDKUNDRF             PIC X(10).                                   
001700*                                 KUNDENS REFERENS (ORDERID)              
001800     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001900*                                 KOLLINUMMER                             
002000     03 KVAVIS               PIC S9(7)           COMP-3.                  
002100*                                 AVISERAT ANTAL                          
002200     03 KVANTAL              PIC S9(7)           COMP-3.                  
002300*                                 ANTAL                                   
002400     03 AVVIKELSETYP         PIC X(10).                                   
002500     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002600*                                 ARTIKELSTANDARDPRIS                     
002700     03 KDSORT1              PIC S9              COMP-3.                  
002800*                                 SORTERINGSKOD                           
002900     03 FLINLREP             PIC X.                                       
003000*                                 FLAGGA AVVIKELSERAPPORTER               
003100     03 IDUSER-PIC           PIC X(8).                                    
003200*                                 ANVÄNDARENS SÄKERHETS ID                
003300     03 ADART.                                                            
003400*                                 ARTIKELADRESS I LAGRET                  
003500        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
003600*                                 LAGEROMRÅDE                             
003700        05 ADGANG            PIC S9(3)           COMP-3.                  
003800*                                 GÅNG                                    
003900        05 ADPLATS           PIC S9(5)           COMP-3.                  
004000*                                 LAGERPLATSNUMMER                        
004100     03 IDDISTR              PIC S9(5)           COMP-3.                  
004200*                                 DISTRIKTNUMMER                          
004300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
004400*                                 PRODUKTIONSNUMMER                       
004500     03 IDORDER              PIC S9(7)           COMP-3.                  
004600*                                 VOLVO PARTS ORDERNUMMER                 
004700     03 IDPLKLST             PIC S9(3)           COMP-3.                  
004800*                                 PLOCKLISTNUMMER                         
004900     03 KDORDKL              PIC S9              COMP-3.                  
005000*                                 ORDERKLASS                              
005100     03 KDFRAKT              PIC S9(3)           COMP-3.                  
005200*                                 FRAKTSÄTT DC TILL KUND                  
005300     03 IDPRC.                                                            
005400*                                 PRODUKTIONSKANAL                        
005500        05 IDPRCBAS          PIC X(3).                                    
005600*                                 PRC-BAS                                 
005700        05 IDPRCVAR          PIC X.                                       
005800*                                 PRC-VARIANT                             
005900     03 KDPRCGRP             PIC X(5).                                    
006000*                                 PRODUKTIONSKANALSGRUPP                  
006100     03 DAREGDAT             PIC 9(8).                                    
006200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
006300     03 IDUSER-BIN           PIC X(8).                                    
006400*                                 ANVÄNDARENS SÄKERHETS ID                
006500*** END OF VILMAII-COPY LENGTH= 119 BYTES                                 
