000100 01  W21720-SORT.                                                         
000200*                                 COPY-TEXT F÷R SORT I W21720             
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 ADGANG               PIC S9(3)           COMP-3.                  
000800*                                 G≈NG                                    
000900     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001000*                                 LAGEROMR≈DE                             
001100     03 ADPLATS              PIC S9(5)           COMP-3.                  
001200*                                 LAGERPLATSNUMMER                        
001300     03 BEART                PIC X(25).                                   
001400*                                 ARTIKELBENƒMNING                        
001500     03 BEFT                 PIC S9(3)           COMP-3.                  
001600*                                 F÷RPACKNINGSTYP                         
001700     03 BELEV                PIC X(35).                                   
001800*                                 LEVERANT÷RSNAMN                         
001900     03 DAPBPLAN             PIC 9(8).                                    
002000*                                 DATUM KVPB-PLAN GILTIG TOM              
002100     03 DAPUBL               PIC 9(8).                                    
002200*                                 PUBLICERINGSDATUM PER ART/LAND          
002300     03 DASEASON             PIC 9(8).                                    
002400*                                 DATUM RESEASON-LEDTID GILTIG TO         
002500*                                 M                                       
002600     03 FLAVT                PIC X.                                       
002700     03 FLERSDAT-VIPS        PIC X.                                       
002800     03 FLJIT                PIC X.                                       
002900*                                 JUST-IN-TIME FLAGGA                     
003000     03 FLLSRDEL             PIC X.                                       
003100*                                 LEVERERAS SOM RESDEL                    
003200     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
003300*                                 DEL AV AK P≈ VƒG                        
003400     03 KVAKS-SDC            PIC S9(7)           COMP-3.                  
003500*                                 DEL AV AK SOM LIGGER I SDC              
003600     03 KDARTURS             PIC X(2).                                    
003700*                                 ARTIKELURSPRUNGSKOD                     
003800     03 KVAVROP-SUM-VV       PIC S9(7)           COMP-3.                  
003900*                                 AVROPSKVANTITET                         
004000     03 KDFORP.                                                           
004100*                                 F÷RPACKNINGSKOD                         
004200        05 KDFORPPL          PIC 9.                                       
004300*                                 F÷RPACKNINGSPLATS                       
004400        05 KDFORPGP          PIC 9(2).                                    
004500*                                 F÷RPACKNINGSGRUPP                       
004600        05 KDFORPUF          PIC 9.                                       
004700*                                 UPPRƒKNINGSFAKTOR                       
004800     03 KVLS                 PIC S9(7)           COMP-3.                  
004900*                                 LAGERSALDO                              
005000     03 KVPB-SUM-VV          PIC S9(6)V9(1)      COMP-3.                  
005100*                                 PERIODBEHOV (PROGNOS)                   
005200     03 KVRESS               PIC S9(7)           COMP-3.                  
005300*                                 RESERVERAT ANTAL ARTIKLAR               
005400     03 KVRORAD              PIC S9(5)           COMP-3.                  
005500*                                 ANTAL RESTORDER-RADER                   
005600     03 KVROS-BULK           PIC S9(7)           COMP-3.                  
005700*                                 RESTORDERSALDO, KLASS 2-4               
005800     03 KVROS-DAG            PIC S9(7)           COMP-3.                  
005900*                                 RESTORDERSALDO, KLASS 1                 
006000     03 KVSLAP-SUM           PIC S9(7)           COMP-3.                  
006100*                                 AVROPSKVANTITET                         
006200     03 KVAVIS-NOT-REC       PIC S9(7)           COMP-3.                  
006300*                                 AVISERAT ANTAL                          
006400     03 KVAVIS-BSKKVAR       PIC S9(7)           COMP-3.                  
006500*                                 AVISERAT ANTAL                          
006600     03 KVAVIS-FORAVIS       PIC S9(7)           COMP-3.                  
006700*                                 AVISERAT ANTAL                          
006800     03 KVVECKOR-LT          PIC S9(3)           COMP-3.                  
006900*                                 ANTAL VECKOR LEDTID                     
007000     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
007100*                                 PERIODBEHOV REFILLING                   
007200     03 KVPBREOI             PIC S9(6)V9(1)      COMP-3.                  
007300*                                 PERIODBEHOV F÷R REFILL OI               
007400     03 KVPB-PLAN            PIC S9(6)V9(1)      COMP-3.                  
007500*                                 PLANERAT PERIODBEHOV                    
007600     03 KVPB-TREND           PIC S9(6)V9(1)      COMP-3.                  
007700*                                 PERIODTRENDVƒRDE                        
007800     03 KVVECKOR-TREND       PIC S9(3)           COMP-3.                  
007900*                                 ANTAL VECKOR TRENDVƒRDE                 
008000     03 KVOI-12-RULL         PIC S9(7)           COMP-3.                  
008100*                                 ORDERING≈NG I STYCK PER TIDSENH         
008200     03 KVOI-YEAR-0          PIC S9(7)           COMP-3.                  
008300*                                 ORDERING≈NG I STYCK PER TIDSENH         
008400     03 KVOI-YEAR-1          PIC S9(7)           COMP-3.                  
008500*                                 ORDERING≈NG I STYCK PER TIDSENH         
008600     03 KVOI-YEAR-2          PIC S9(7)           COMP-3.                  
008700*                                 ORDERING≈NG I STYCK PER TIDSENH         
008800     03 KVOI-YEAR-3          PIC S9(7)           COMP-3.                  
008900*                                 ORDERING≈NG I STYCK PER TIDSENH         
009000     03 KVOI-YEAR-4          PIC S9(7)           COMP-3.                  
009100*                                 ORDERING≈NG I STYCK PER TIDSENH         
009200     03 KVOI-YEAR-5          PIC S9(7)           COMP-3.                  
009300*                                 ORDERING≈NG I STYCK PER TIDSENH         
009400     03 KDERS                PIC S9(3)           COMP-3.                  
009500*                                 ERSƒTTNINGSKOD                          
009600     03 KDFPKPRI             PIC X.                                       
009700*                                 OM F÷RPACKNING ING≈R I ARTPRIS          
009800     03 KDSORT               PIC X(2).                                    
009900*                                 SORT-KOD                                
010000     03 KDFREKKL             PIC X.                                       
010100*                                 FREKVENSKLASS                           
010200     03 KDPRISKL             PIC X.                                       
010300*                                 PRISKLASS                               
010400     03 KDPRODSL             PIC S9(3)           COMP-3.                  
010500*                                 PRODUKTSLAG                             
010600     03 KVREFBER             PIC S9(7)           COMP-3.                  
010700*                                 BERƒKNAD REFILLINGKVANTITET             
010800     03 KVEOQ                PIC S9(7)           COMP-3.                  
010900*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
011000*                                 ET                                      
011100     03 KVPALL               PIC S9(7)           COMP-3.                  
011200*                                 ANTAL I PALL                            
011300     03 KVULOAD              PIC S9(7)           COMP-3.                  
011400*                                 MIN ENHETSLAST FR≈N LEVERANT÷R          
011500     03 KVREFOVL             PIC S9(7)           COMP-3.                  
011600*                                 BERƒKNAD ÷VERLAGERPUNKT                 
011700     03 KVSPANT              PIC S9(7)           COMP-3.                  
011800*                                 SPƒRRAT ANTAL                           
011900     03 KVSLAGER             PIC S9(7)           COMP-3.                  
012000*                                 SƒKERHETSLAGER                          
012100     03 KVTILLG-TOT          PIC S9(7)           COMP-3.                  
012200*                                 LAGERTILLG≈NG CDC TOTALT                
012300     03 KDLEVPLF             PIC X.                                       
012400*                                 KOD F÷R LEVPLAN-GODKƒNNANDE             
012500     03 KDKRSTA              PIC X.                                       
012600*                                 KONTROLLRAPPORT STATUS                  
012700     03 IDPROJ               PIC X(4).                                    
012800*                                 PARTS PROJEKTIDENTITET                  
012900     03 IDKAT                OCCURS 3 TIMES                               
013000                             PIC X(5).                                    
013100*                                 KATALOGBETECKNING                       
013200     03 IDKR                 PIC 9(5).                                    
013300*                                 KONTROLLRAPPORT NUMMER                  
013400     03 IDLEVNR              PIC X(5).                                    
013500*                                 LEVERANT÷RNUMMER                        
013600     03 IDLEVNR-SHIP         PIC X(5).                                    
013700*                                 SKEPPANDE LEVERANT÷R                    
013800     03 IDLEVBSK             PIC S9              COMP-3.                  
013900*                                 TYP AV LEVERANSBESKEDSTEXT              
014000     03 IDANSK               PIC S9(3)           COMP-3.                  
014100*                                 ANSKAFFARNUMMER                         
014200     03 IDBERED              PIC S9(3)           COMP-3.                  
014300*                                 BEREDARENUMMER                          
014400     03 IDREFTAB             PIC X.                                       
014500*                                 IDENTITET REFILLTABELL                  
014600     03 IDINK                PIC X(4).                                    
014700*                                 INK÷PARNUMMER                           
014800     03 IDARTNR-EMBQ0        PIC S9(9)           COMP-3.                  
014900*                                 EMBALLAGEARTIKELNR F÷R Q0               
015000     03 IDARTNR-EMBQ1        PIC S9(9)           COMP-3.                  
015100*                                 EMBALLAGEARTIKELNR F÷R Q1               
015200     03 IDARTNR-EMBQ2        PIC S9(9)           COMP-3.                  
015300*                                 EMBALLAGEARTIKELNR F÷R Q2               
015400     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
015500*                                 FUNKTIONSGRUPP                          
015600     03 PRMATRL              PIC S9(7)V9(2)      COMP-3.                  
015700*                                 FAST PRIS UNDER L÷PANDE ≈R              
015800     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
015900*                                 MEDELVƒRDESKOSTNAD I UTL.VALUTA         
016000     03 RESEASON-PLAN        OCCURS 12 TIMES                              
016100                             PIC S9V9(2)         COMP-3.                  
016200*                                 SƒSONGSINDEX INKLUSIVE REFILL           
016300     03 RESERVG-NTO          PIC S9(3)V9(2)      COMP-3.                  
016400*                                 PROCENT SERVICEGRAD                     
016500     03 SUINKORD             PIC S9(18)          COMP-3.                  
016600*                                 SUMMA INKOMNA ORDERRADER PER            
016700*                                 BRYTBEGREPP                             
016800     03 SUAVBRP              PIC S9(5)V9(2)      COMP-3.                  
016900*                                 SUMMA AVBOKAD MƒNGD,DEL AV RAD          
017000     03 TILEVBSK-AVS         PIC S9(7)           COMP-3.                  
017100*                                 LEV. BESK. AVS. DAT.(≈≈MMDD)            
017200     03 TILEVBSK-INL         PIC S9(7)           COMP-3.                  
017300*                                 LEV. BESK. INLEV. DAT(≈≈MMDD)           
017400     03 TILEVBSK-INL-DC      PIC S9(7)           COMP-3.                  
017500*                                 LEV. BESK. INLEV. DAT(≈≈MMDD)           
017600     03 TIBORT-INFO          PIC S9(7)           COMP-3.                  
017700*                                 BORTTAGSDATUM  (≈≈MMDD)                 
017800     03 TILEVBSK-DISP        PIC S9(7)           COMP-3.                  
017900*                                 LEV. BESK. DISPONIBEL(≈≈MMDD)           
018000     03 TILEVBSK-DISP-DC     PIC S9(7)           COMP-3.                  
018100*                                 LEV. BESK. DISPONIBEL(≈≈MMDD)           
018200     03 TILEVDAG             OCCURS 5 TIMES                               
018300                             PIC S9              COMP-3.                  
018400*                                 AVSƒNDNINGSDAG INOM VECKA               
018500     03 LATESTDEL            OCCURS 5 TIMES.                              
018600        05 TIAVIDAT-LATE     PIC S9(7)           COMP-3.                  
018700*                                 AVISERINGSDATUM (YYMMDD)                
018800        05 IDKUNDRF-LATE     PIC X(10).                                   
018900*                                 KUNDENS REFERENS (ORDERID)              
019000        05 KVANTAL-LATE      PIC S9(7)           COMP-3.                  
019100*                                 ANTAL                                   
019200     03 TIDATUM-TREND        PIC S9(7)           COMP-3.                  
019300*                                 JUSTERAD TREND AAMMDD                   
019400     03 TIREFSTO-LOC         PIC S9(7)           COMP-3.                  
019500*                                 REF.STOPP T.O.M DATUM (NDC>LDC)         
019600     03 TIFINLV              PIC S9(5)           COMP-3.                  
019700*                                 PUBLICERINGSVECKA, (≈≈VVD  D=1)         
019800     03 TIURPROD             PIC S9(5)           COMP-3.                  
019900*                                 DATUM UTG≈TT UR PROD   (≈≈VV)           
020000     03 TIREFPAF             PIC S9(7)           COMP-3.                  
020100*                                 DATUM MANUELL P≈FYLLNADSKVANT           
020200     03 TIMANSEC             PIC S9(7)           COMP-3.                  
020300*                                 DATUM MANUELL SƒK-LAGER(≈≈MMDD)         
020400     03 VKART                PIC S9(7)           COMP-3.                  
020500*                                 ARTIKELVIKT (G)                         
020600     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
020700*                                 ARTIKELVOLYM NETTO (CM3)                
020800     03 KVVORKO              PIC S9(7)           COMP-3.                  
020900*                                 VOR-K÷ KVANT                            
021000*** END OF VILMAII-COPY LENGTH= 519 BYTES                                 
