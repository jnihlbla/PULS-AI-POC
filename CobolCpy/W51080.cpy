000100 01  W51080.                                                              
000200*                                 FÖR-POST FÖR TRANSAR TILL LEVA1         
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDFS                 PIC X(8).                                    
000800*                                 FÖLJESEDELSNUMMER ENL ODETTE            
000900     03 IDINK                PIC X(4).                                    
001000*                                 INKÖPARNUMMER                           
001100     03 IDKONTO              PIC 9(10).                                   
001200*                                 KONTO                                   
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
001600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001700*                                 (0VVDLLLLK)                             
001800     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001900*                                 PRODUKTSLAG                             
002000     03 KDRT                 PIC S9(3)           COMP-3.                  
002100*                                 REDOVISNINGSTYP                         
002200     03 KDSORT               PIC X(2).                                    
002300*                                 SORT-KOD                                
002400     03 KDTIPPR              PIC S9              COMP-3.                  
002500*                                 TIPPAT PRIS KOD                         
002600     03 KDVALISO             PIC X(3).                                    
002700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002800     03 KVAVIS               PIC S9(7)           COMP-3.                  
002900*                                 AVISERAT ANTAL                          
003000     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
003100*                                 BESTÄLLNINGSPRIS I KRONOR               
003200     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
003300*                                 INKÖPSPRIS                              
003400     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
003500*                                 TULLFAKTOR                              
003600     03 PRARTBEL-PR          PIC S9(8)V9(5)      COMP-3.                  
003700*                                 DETTA BESTÄLLNINGSPRIS                  
003800*                                 (I LEVERANTÖRENS VALUTA)                
003900     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
004000*                                 AVISERINGSDATUM (YYMMDD)                
004100     03 DAREGDAT             PIC 9(8).                                    
004200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004300     03 TIKLOCK              PIC S9(9)           COMP-3.                  
004400*                                 KLOCKSLAG (TTMMSSTH)                    
004500     03 FLLSBOK              PIC X.                                       
004600*                                 LAGERAVBOKNING                          
004700     03 IDDISTR              PIC S9(5)           COMP-3.                  
004800*                                 DISTRIKTNUMMER                          
004900     03 FLAVVINL             PIC X.                                       
005000*                                 AVVIKELSE FÖR INLEVERANS                
005100     03 KDINLAVV             PIC X.                                       
005200*                                 TYP AV AVVIKELSE                        
005300     03 FLDIRLEV             PIC X.                                       
005400*                                 DIREKTLEVERANS ?                        
005500     03 PRHEMTAG             PIC S9(7)V9(2)      COMP-3.                  
005600*                                 HEMTAGNINGSKOSTNAD                      
005700     03 IDAVTAL              PIC 9(12).                                   
005800*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
005900*                                 PPP   = INKÖPARNR (PREFIX)              
006000*                                 BBBBB = BESTÄLLARNR                     
006100*                                 SSS   = SUFFIX                          
006200     03 IDFTG                PIC 9(2).                                    
006300*                                 FÖRETAGSID EKONOM REDOVISNING           
006400     03 FILLER               PIC X(8).                                    
006500*** END OF VILMAII-COPY LENGTH= 125 BYTES                                 
