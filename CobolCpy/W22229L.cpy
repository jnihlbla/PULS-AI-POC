000100 01  W22229L-CTX.                                                         
000200*                                 LOG FILE KVMAD PARAMETERS               
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000600*                                 PRODUKTSLAG                             
000700     03 DAPBPLAN             PIC 9(8).                                    
000800*                                 DATUM KVPB-PLAN GILTIG TOM              
000900     03 KVPB-PLAN            PIC S9(6)V9(1)      COMP-3.                  
001000*                                 PLANERAT PERIODBEHOV                    
001100     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
001200*                                 SEPARAT PERIODBEHOV                     
001300     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
001400*                                 SATS-PERIODBEHOV                        
001500     03 KVMAD-TOT            PIC S9(6)V9(1)      COMP-3.                  
001600*                                 TOTALT PROGNOSFEL                       
001700     03 RVPROFEL             PIC S9(3)           COMP-3.                  
001800*                                 ANTAL STORA PROGNOSFEL                  
001900     03 KVUTJFEL             PIC S9(6)V9(1)      COMP-3.                  
002000*                                 UTJÄMNAT FEL                            
002100     03 TIRP                 PIC S9(2)           COMP-3.                  
002200*                                 REDOVISNINGSPERIOD                      
002300*                                 12 PER ÅR                               
002400     03 RESEASON             PIC S9V9(2)         COMP-3.                  
002500*                                 SÄSONGSINDEX                            
002600     03 FL-OI-ID-EQ-TRANS-ID PIC X.                                       
002700*                                 ALLMÄN FLAGGA                           
002800     03 KVOI-PROG            PIC S9(7)           COMP-3.                  
002900*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
003000     03 KVOI-REFILL          PIC S9(7)           COMP-3.                  
003100*                                 ORDERINGÅNG LEV FRÅN REFILL             
003200     03 KVOI-SATS            PIC S9(7)           COMP-3.                  
003300*                                 ORDERINGÅNG SATSFÖRBRUKNING             
003400     03 TIAAPP               PIC S9(5)           COMP-3.                  
003500*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
003600*                                 12 PER ÅR                               
003700*                                 NUMERA ÄR DETTA "PV-PERIOD"             
003800     03 KDVALUES-USED        PIC X(8).                                    
003900*                                 FUNKTIONSNAMN                           
004000     03 FLMASK-CALCULATED    PIC X.                                       
004100*                                 ALLMÄN FLAGGA                           
004200     03 KVPB-PLAN-MASK       PIC S9(6)V9(1)      COMP-3.                  
004300*                                 PLANERAT PERIODBEHOV                    
004400     03 KVPB-TOT-MASK        PIC S9(8)V9(1)      COMP-3.                  
004500*                                 ARTIKELVOLYM (CM3)                      
004600     03 PROGFEL-TOT-MASK     PIC S9(6)V9(5)      COMP-3.                  
004700*                                 VALUTAKURS                              
004800     03 KVUTJFEL-MASK        PIC S9(6)V9(1)      COMP-3.                  
004900*                                 UTJÄMNAT FEL                            
005000     03 KVMAD-TOT-MIN-MASK   PIC S9(6)V9(1)      COMP-3.                  
005100*                                 TOTALT PROGNOSFEL                       
005200     03 KVMAD-TOT-MASK       PIC S9(6)V9(1)      COMP-3.                  
005300*                                 TOTALT PROGNOSFEL                       
005400     03 FLMAN-CALCULATED     PIC X.                                       
005500*                                 ALLMÄN FLAGGA                           
005600     03 KVPB-PLAN-MAN        PIC S9(6)V9(1)      COMP-3.                  
005700*                                 PLANERAT PERIODBEHOV                    
005800     03 KVPB-TOT-MAN         PIC S9(8)V9(1)      COMP-3.                  
005900*                                 ARTIKELVOLYM (CM3)                      
006000     03 PROGFEL-TOT-MAN      PIC S9(6)V9(5)      COMP-3.                  
006100*                                 VALUTAKURS                              
006200     03 KVUTJFEL-MAN         PIC S9(6)V9(1)      COMP-3.                  
006300*                                 UTJÄMNAT FEL                            
006400     03 KVMAD-TOT-MIN-MAN    PIC S9(6)V9(1)      COMP-3.                  
006500*                                 TOTALT PROGNOSFEL                       
006600     03 KVMAD-TOT-MAN        PIC S9(6)V9(1)      COMP-3.                  
006700*                                 TOTALT PROGNOSFEL                       
006800*** END OF VILMAII-COPY LENGTH= 121 BYTES                                 
