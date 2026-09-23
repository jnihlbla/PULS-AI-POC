000100* GENERATION OF COBOL HOST STRUCTURE FROM T01INRE-TAB                     
000200  01 T01INRE.                                                             
000300*              T01INRE                                                    
000400   03 IDLEGSEL        PIC X(4).                                           
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 IDLANDX3-SEND   PIC X(3).                                           
000700*              LANDKOD SƒNDANDE LAND                                      
000800   03 IDLANDX3-REC    PIC X(3).                                           
000900*              LANDKOD MOTTAGANDE LAND                                    
001000   03 KDSTATUS        PIC S9(3) COMP-3.                                   
001100*              STATUSKOD          KDSTATUS-002                            
001200   03 FLEXPORT        PIC X(1).                                           
001300*              EXPORTFAKTURA                                              
001400   03 FLVAT           PIC X(1).                                           
001500*              MOMS P≈ FAKTURA                                            
001600   03 FLVAT-PRIV      PIC X(1).                                           
001700*              MOMS P≈ FAKTURA                                            
001800   03 FLVATREP        PIC X(1).                                           
001900*              MOMSRAPPORT                                                
002000   03 FLINTREP        PIC X(1).                                           
002100*              INTRASTATRAPPORTERING                                      
002200   03 KDVAT           PIC X(2).                                           
002300*              MOMSKOD                                                    
002400   03 DAREGDAT        PIC X(8).                                           
002500*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
002600   03 DAUPPDAT        PIC X(8).                                           
002700*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
002800   03 DADELDAT        PIC X(8).                                           
002900*              BORTTAGSDATUM      (≈≈≈≈MMDD)                              
003000   03 IDUSER          PIC X(8).                                           
003100*              ANVƒNDARENS SƒKERHETS ID                                   
003200   03 KDVAT-SERV      PIC X(2).                                           
003300*              MOMSKOD                                                    
003400   03 FLCUSREP        PIC X(1).                                           
003500*              TULLRAPPORT                                                
003600   03 BETEXT-1        PIC X(100).                                         
003700   03 BETEXT-2        PIC X(100).                                         
003800   03 BETEXT-3        PIC X(100).                                         
003900   03 BETEXT-4        PIC X(100).                                         
004000   03 BETEXT-5        PIC X(100).                                         
004100   03 BETEXT-6        PIC X(100).                                         
004200   03 BETEXT-7        PIC X(100).                                         
004300   03 BETEXT-8        PIC X(100).                                         
004400   03 BETEXT-9        PIC X(100).                                         
004500   03 BETEXT-10       PIC X(100).                                         
004600   03 BETEXT-11       PIC X(100).                                         
004700   03 BETEXT-12       PIC X(100).                                         
004800   03 BETEXT-13       PIC X(100).                                         
004900   03 BETEXT-14       PIC X(100).                                         
005000   03 BETEXT-15       PIC X(100).                                         
005100   03 BETEXT-16       PIC X(100).                                         
005200   03 BETEXT-17       PIC X(100).                                         
005300   03 BETEXT-18       PIC X(100).                                         
005400   03 BETEXT-19       PIC X(100).                                         
005500   03 BETEXT-20       PIC X(100).                                         
005600   03 BETEXT-21       PIC X(100).                                         
005700   03 BETEXT-22       PIC X(100).                                         
005800   03 BETEXT-23       PIC X(100).                                         
005900   03 BETEXT-24       PIC X(100).                                         
006000   03 BETEXT-25       PIC X(100).                                         
006100   03 BETEXT-26       PIC X(100).                                         
006200   03 BETEXT-27       PIC X(100).                                         
006300   03 BETEXT-28       PIC X(100).                                         
006400   03 BETEXT-29       PIC X(100).                                         
006500   03 BETEXT-30       PIC X(100).                                         
006600   03 BETEXT-31       PIC X(100).                                         
006700   03 BETEXT-32       PIC X(100).                                         
006800   03 BETEXT-33       PIC X(100).                                         
006900   03 BETEXT-34       PIC X(100).                                         
007000   03 BETEXT-35       PIC X(100).                                         
007100   03 BETEXT-36       PIC X(100).                                         
007200   03 BETEXT-37       PIC X(100).                                         
007300   03 BETEXT-38       PIC X(100).                                         
007400   03 BETEXT-39       PIC X(100).                                         
007500   03 BETEXT-40       PIC X(100).                                         
007600*                                                                         
007700*** END OF VILMAII-COPY LENGTH= 4054 OLD LENGTH=                          
