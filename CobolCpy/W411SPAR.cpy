000100 01  SPAR-W411SPAR.                                                       
000200*                                 LÄNKAREA TILL W411SPAR -                
000300*                                 KONTROLLERA AV SPÄRRAR                  
000400     03 SPAR-BERADREF        PIC X(10).                                   
000500*                                 KUNDENS RADREFERENS                     
000600     03 SPAR-BEKUNDRF        PIC X(15).                                   
000700*                                 KUNDENS REFERENS                        
000800     03 SPAR-FLAVRART        PIC X.                                       
000900*                                 AVROPSARTIKEL                           
001000     03 SPAR-FLEMBORD        PIC X.                                       
001100*                                 EMBALLAGEORDER ?                        
001200     03 SPAR-FLFORBI         PIC X.                                       
001300*                                 FÖRBIORDERFLAGGA                        
001400     03 SPAR-FLIART          PIC X.                                       
001500*                                 ARTIKELN INGÅR I SATS                   
001600     03 SPAR-FLLSRDEL        PIC X.                                       
001700*                                 LEVERERAS SOM RESDEL                    
001800     03 SPAR-FLMARKSP        PIC X.                                       
001900*                                 MARKNADSSPÄRR                           
002000     03 SPAR-FLORDSPE        PIC X.                                       
002100*                                 SPECIALORDERFLAGGA                      
002200     03 SPAR-FLOVRLEV        PIC X.                                       
002300*                                 ÖVERLEVERANS                            
002400     03 SPAR-FLRADREF        PIC X.                                       
002500*                                 KOMPLETTERANDE INFO. KRÄVS              
002600     03 SPAR-FLRESTN         PIC X.                                       
002700*                                 RESTNOTERING ?                          
002800     03 SPAR-FLSDCLEV        PIC X.                                       
002900*                                 LEVERANSSTYRNING SDC                    
003000     03 SPAR-IDARTNR         PIC S9(9)           COMP-3.                  
003100*                                 ARTIKELNUMMER                           
003200     03 SPAR-IDDISTR         PIC S9(5)           COMP-3.                  
003300*                                 DISTRIKTNUMMER                          
003400     03 SPAR-IDKUNDRF-RO     PIC X(10).                                   
003500*                                 KUND REF PÅ RO                          
003600     03 SPAR-IDDC            PIC X(2).                                    
003700*                                 IDENTIFIERARE LAGER                     
003800     03 SPAR-IDSYSTEM        PIC X(4).                                    
003900*                                 VOLVO VCCS SYSTEMNUMMER                 
004000     03 SPAR-KDERS-UTG       PIC S9(3)           COMP-3.                  
004100*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
004200     03 SPAR-KDERS           PIC S9(3)           COMP-3.                  
004300*                                 ERSÄTTNINGSKOD                          
004400     03 SPAR-KDFAKTYP        PIC X.                                       
004500*                                 FAKTURATYP                              
004600     03 SPAR-KDLEVSP         PIC S9(3)           COMP-3.                  
004700*                                 SPÄRRKOD LEVERANS                       
004800     03 SPAR-KDORDBEH        PIC S9              COMP-3.                  
004900*                                 STATUSKOD ORDERBEHANDLING               
005000     03 SPAR-KDORDKL         PIC S9              COMP-3.                  
005100*                                 ORDERKLASS                              
005200     03 SPAR-KDPRODSL        PIC S9(3)           COMP-3.                  
005300*                                 PRODUKTSLAG                             
005400     03 SPAR-KDPRTYP         PIC X.                                       
005500*                                 TYP AV PRISTILLÄMPNING                  
005600     03 SPAR-KDSORT          PIC X(2).                                    
005700*                                 SORT-KOD                                
005800     03 SPAR-KDTPOTYP        PIC S9              COMP-3.                  
005900*                                 TYP AV TIDPLANERAD ORDER                
006000     03 SPAR-KDUART          PIC X.                                       
006100*                                 UNDANTAGSARTIKEL                        
006200     03 SPAR-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
006300*                                 ARTIKELSTANDARDPRIS                     
006400     03 SPAR-TIFINLV         PIC S9(5)           COMP-3.                  
006500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
006600     03 SPAR-TIRODAT         PIC S9(7)           COMP-3.                  
006700*                                 RESTORDERDATUM         (ÅÅMMDD)         
006800     03 SPAR-TITPO           PIC S9(7)           COMP-3.                  
006900*                                 PLANERAD ORDERDATUM                     
007000     03 SPAR-KDORDBEK        PIC 9(2).                                    
007100*                                 ORDERBEKRÄFTELSEKOD                     
007200     03 SPAR-IDKUNDNR        PIC S9(7)           COMP-3.                  
007300*                                 KUNDNUMMER                              
007400     03 SPAR-TIREPDAT        PIC S9(7)           COMP-3.                  
007500*                                 REPAIR DATE                             
007600     03 SPAR-FLPUBCDC        PIC X.                                       
007700*                                 PUBLICERINGSDATUM CDC FINNS             
007800*** END OF VILMAII-COPY LENGTH= 103 BYTES                                 
