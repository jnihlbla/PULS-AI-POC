000100 01  UPD-W2214A.                                                          
000200*                                 UPPDAT-FIL FÖR LEVERANSPLANER           
000300*                                                                         
000400*                                 POSTTYP = 001 UPPDAT CLAG               
000500*                                 KVSLAGER, KVEOQ,KVQ, KVQ-JUST,          
000600*                                 KDLPSP, KVSLUTKP, TILPSP,               
000700*                                 KDLEVPLF, DAPBPLAN, KVPB-PLAN           
000800*                                 DASEASON, RESEASON-PLAN(1--12)          
000900*                                                                         
001000*                                 POSTTYP = 002 BORTTAG KOPPL. LP         
001100*                                 IDARTNR, IDDC                           
001200*                                                                         
001300*                                 POSTTYP = 003 BORTTAG OMSPEC            
001400*                                 IDARTNR, IDDC, IDLEVNR,                 
001500*                                                                         
001600*                                 POSTTYP = 004 BORTTAG AVROP             
001700*                                 IDARTNR, IDDC                           
001800*                                                                         
001900     03 UPD-IDPTYP           PIC X(3).                                    
002000*                                 POSTTYP                                 
002100*                                 RECORD TYPE                             
002200     03 UPD-IDARTNR          PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500     03 UPD-IDDC             PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700*                                 WAREHOUSE IDENTIFIER                    
002800     03 UPD-IDLEVNR          PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003100     03 UPD-FLMANQ           PIC X.                                       
003200*                                 MANUELL HEMTAGNINGSKVANTITET            
003300     03 UPD-KDAVT            PIC S9              COMP-3.                  
003400*                                 AVTALSMÄRKNING                          
003500*                                 AGREEMENT CODE                          
003600     03 UPD-KDKSP            PIC S9              COMP-3.                  
003700*                                 KÖPSPÄRR                                
003800*                                 PURCHASE BLOCKING CODE                  
003900     03 UPD-KDLPSP           PIC S9              COMP-3.                  
004000*                                 LEVERANSPLANESPÄRR                      
004100     03 UPD-KDVVKL           PIC S9              COMP-3.                  
004200*                                 VOLYMVÄRDESKLASS                        
004300*                                 VOLUME VALUE CLASS                      
004400     03 UPD-KDOPPLAN         PIC X.                                       
004500*                                 OPTIMAL PLAN INOM FRYSTID               
004600*                                 OPTIMAL PLAN WITHIN FREEZTIME           
004700     03 UPD-KVAP             PIC S9(7)           COMP-3.                  
004800*                                 ANNULLATIONSPUNKT                       
004900     03 UPD-KVBK             PIC S9(7)           COMP-3.                  
005000*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
005100     03 UPD-KVKP             PIC S9(7)           COMP-3.                  
005200*                                 KÖPPUNKT                                
005300     03 UPD-KVOVERF          PIC S9(7)           COMP-3.                  
005400*                                 ÖVERFÖRINGSSALDO                        
005500     03 UPD-KVQ              PIC S9(7)           COMP-3.                  
005600*                                 EKONOMISK HEMTAGNINGSKVANTITET          
005700     03 UPD-KVQ-JUST         PIC S9(7)           COMP-3.                  
005800*                                 NY EKON HEMTAGNINGSKVANTITET            
005900     03 UPD-KVSLUTKP         PIC S9(7)           COMP-3.                  
006000*                                 SLUTKÖPSSALDO                           
006100     03 UPD-TIBESRPT         PIC S9(5)           COMP-3.                  
006200*                                 DATUM FÖR BESTÄLLNINGSRAPPORT           
006300*                                 (ÅÅVV)                                  
006400     03 UPD-TIBESRPT-PAAM    PIC S9(5)           COMP-3.                  
006500*                                 PÅMINNELSEDATUM FÖR                     
006600*                                 BESTÄLLNINGSRAPPORT (ÅÅVV)              
006700     03 UPD-TILPSP           PIC S9(5)           COMP-3.                  
006800*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
006900     03 UPD-TIQJUST          PIC S9(5)           COMP-3.                  
007000*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
007100     03 UPD-KVDAGAR-INLEV    PIC S9(3)           COMP-3.                  
007200*                                 INLEVERANSTID     (ANTAL DAGAR)         
007300     03 UPD-KVDAGAR-FFH      PIC S9(3)           COMP-3.                  
007400*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
007500     03 UPD-KVVECKOR-LT      PIC S9(3)           COMP-3.                  
007600*                                 ANTAL VECKOR LEDTID                     
007700     03 UPD-KVVECKOR-FT      PIC S9(3)           COMP-3.                  
007800*                                 ANTAL VECKOR FRYSNINGSTID               
007900     03 UPD-KVVECKOR-BT      PIC S9(3)           COMP-3.                  
008000*                                 ANTAL VECKOR BESTÄLLNINGSTID            
008100     03 UPD-KVVECKOR-AT      PIC S9(3)           COMP-3.                  
008200*                                 ANTAL VECKOR ANSKAFFNINGSTID            
008300     03 UPD-FLJIT            PIC X.                                       
008400*                                 JUST-IN-TIME FLAGGA                     
008500*                                 JUST-IN-TIME FLAG                       
008600     03 UPD-KDFREKKL         PIC X.                                       
008700*                                 FREKVENSKLASS                           
008800*                                 FREQ. CLASS                             
008900     03 UPD-KDPRISKL         PIC X.                                       
009000*                                 PRISKLASS                               
009100*                                 PRICE CLASS                             
009200     03 UPD-KDLEVPLF         PIC X.                                       
009300*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
009400*                                 CODE FOR APPROVAL OF SCHEDULE P         
009500*                                 ROPOSAL                                 
009600     03 UPD-IDPLANGR-LEV     PIC S9              COMP-3.                  
009700*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
009800     03 UPD-FLMPB            PIC X.                                       
009900*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
010000     03 UPD-KVMAD-SEP        PIC S9(6)V9(1)      COMP-3.                  
010100*                                 SEPARAT PROGNOSFEL                      
010200     03 UPD-KVMAD-TOT        PIC S9(6)V9(1)      COMP-3.                  
010300*                                 TOTALT PROGNOSFEL                       
010400     03 UPD-KVMP             PIC S9(7)           COMP-3.                  
010500*                                 MAXPUNKT                                
010600*                                 MAXIMUM POINT                           
010700     03 UPD-KVPB-VESL        PIC S9(6)V9(1)      COMP-3.                  
010800*                                 GÄLLANDE PB VID VECKOSLUT               
010900     03 UPD-RESLJUST         PIC S9(2)V9(1)      COMP-3.                  
011000*                                 SÄKERHETSLAGER-JUSTERINGFAKTOR          
011100*                                 ADJUSTMENT ALGORITM                     
011200     03 UPD-TISLJUST         PIC S9(5)           COMP-3.                  
011300*                                 VECKA DÅ JUSTERING AV SÄKER-            
011400*                                 HETSLAGER UPPHÖR (ÅÅVV)                 
011500     03 UPD-KVPB-SEP         PIC S9(6)V9(1)      COMP-3.                  
011600*                                 SEPARAT PERIODBEHOV                     
011700*                                 SEPARATE PERIOD REQUIREMENTS            
011800     03 UPD-RVPROURS         PIC S9(3)           COMP-3.                  
011900*                                 ANTAL PROGNOSFEL I FÖLJD                
012000     03 UPD-RVPROFEL         PIC S9(3)           COMP-3.                  
012100*                                 ANTAL STORA PROGNOSFEL                  
012200     03 UPD-FLMANPB          PIC X.                                       
012300*                                 MANUELLT REGISTRERAT PB-TPO             
012400*                                 MANUALLY REGISTRATED PB-TPO             
012500     03 UPD-KVPB-TPO         PIC S9(6)V9(1)      COMP-3.                  
012600*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
012700*                                 PERIODICAL DEMAND TPO1 AND TPO2         
012800*                                                                         
012900     03 UPD-TILTK            PIC S9(5)           COMP-3.                  
013000*                                 LTK-ÄNDRINGSDATUM                       
013100     03 UPD-KDLTK            PIC S9              COMP-3.                  
013200*                                 LAGERTILLHÖRIGHETSKOD                   
013300*                                 STOCK BELONGING CODE                    
013400     03 UPD-KVSLAGER         PIC S9(7)           COMP-3.                  
013500*                                 SÄKERHETSLAGER                          
013600*                                 SAFETY STOCK                            
013700     03 UPD-KVULOAD          PIC S9(7)           COMP-3.                  
013800*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
013900*                                 MIN LOAD FROM SUPPLIER                  
014000     03 UPD-KVEOQ            PIC S9(7)           COMP-3.                  
014100*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
014200*                                 ET                                      
014300     03 UPD-KVSLAGER-OPT     PIC S9(7)           COMP-3.                  
014400*                                 OPTIMALT SÄKERHETSLAGER                 
014500*                                 OPT SAFETY STOCK                        
014600     03 UPD-DAPBPLAN         PIC 9(8).                                    
014700*                                 DATUM KVPB-PLAN GILTIG TOM              
014800*                                 DATE KVPB-PLAN VALID UNTIL              
014900     03 UPD-DASEASON         PIC 9(8).                                    
015000*                                 DATUM RESEASON-LEDTID GILTIG TO         
015100*                                 M                                       
015200*                                 DATE RESEASON-LEDTID VALID UNTI         
015300*                                 L                                       
015400     03 UPD-RESEASON-PLAN    OCCURS 12 TIMES                              
015500                             PIC S9V9(2)         COMP-3.                  
015600*                                 SÄSONGSINDEX INKLUSIVE REFILL           
015700     03 UPD-KDAVROP          PIC S9              COMP-3.                  
015800*                                 AVROPSKOD                               
015900*                                 CALLED                                  
016000*** END OF VILMAII-COPY LENGTH= 174 BYTES                                 
