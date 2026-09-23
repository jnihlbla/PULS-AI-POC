000100 01  UPLP-W2215A.                                                         
000200*                                 UPP-FIL F÷R OMSPEC. LEV.PLAN            
000300*                                 POSTTYP = 001 BORTTAG OMSPEC            
000400*                                 IDARTNR, IDDC                           
000500*                                                                         
000600*                                 POSTTYP = 002 BORTTAG FORSLAG           
000700*                                 IDARTNR, IDDC, KDAVROP                  
000800*                                                                         
000900*                                 POSTTYP = 003 UPDATE WDK611             
001000*                                 IDARTNR, KDLEVPLF, FLSKROT-WLC          
001100*                                 KDLPSP, TILPSP, TIOMSPEC                
001200*                                                                         
001300*                                 POSTTYP = 004 NYUPPL OMSPEC             
001400*                                 IDARTNR, IDDC, IDLEVNR,                 
001500*                                 DASPECST, KDLPORS-TAB(),                
001600*                                 KVBEST-PL, KDPLKOEP                     
001700*                                                                         
001800*                                 POSTTYP = 005 UPPDAT OMSPEC             
001900*                                 IDARTNR, IDDC, IDLEVNR,                 
002000*                                 KDLPORS-TAB(), KVBEST-PL,               
002100*                                 KDPLKOEP                                
002200*                                                                         
002300*                                 POSTTYP = 006 NYUPPL LEV                
002400*                                 IDARTNR, IDDC, IDLEVNR                  
002500*                                                                         
002600*                                 POSTTYP = 007 NYUPPL DAG AVROP          
002700*                                 IDARTNR, IDDC, IDLEVNR,                 
002800*                                 KDAVROP, DAAVROP-AVS, TILEVDAG,         
002900*                                 TIAVRDAT-INL, TIAVRDAT-DISP.            
003000*                                 KVAVROP                                 
003100*                                                                         
003200*                                 POSTTYP = 008 NYUPPL AVROP              
003300*                                 IDARTNR, IDDC, IDLEVNR,                 
003400*                                 KDAVROP, DAAVROP-AVS, TILEVDAG,         
003500*                                 TIAVRDAT-INL, TIAVRDAT-DISP.            
003600*                                 KVAVROP                                 
003700*                                                                         
003800*                                 POSTTYP = 009 FLYTTA BLOC AVROP         
003900*                                 BLOCKADE AVROP SOM SKALL                
004000*                                 FLYTTAS VIA SUB PGM W221BLOC            
004100*                                                                         
004200     03 UPLP-IDPTYP          PIC X(3).                                    
004300*                                 POSTTYP                                 
004400*                                 RECORD TYPE                             
004500     03 UPLP-IDARTNR         PIC S9(9)           COMP-3.                  
004600*                                 ARTIKELNUMMER                           
004700*                                 PART NUMBER                             
004800     03 UPLP-IDDC            PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000*                                 WAREHOUSE IDENTIFIER                    
005100     03 UPLP-IDLEVNR         PIC X(5).                                    
005200*                                 LEVERANT÷RNUMMER                        
005300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005400     03 UPLP-DAAVROP-AVS     PIC 9(6).                                    
005500*                                 AVSƒNDNINGSVECKA (PLANERAD)             
005600*                                 (≈≈≈≈VV)                                
005700     03 UPLP-DASPECST        PIC 9(6).                                    
005800*                                 SPECAD FR.O.M DATUM   (≈≈≈≈VV)          
005900     03 UPLP-KDAVROP         PIC S9              COMP-3.                  
006000*                                 AVROPSKOD                               
006100*                                 CALLED                                  
006200     03 UPLP-KDLEVPLF        PIC X.                                       
006300*                                 KOD F÷R LEVPLAN-GODKƒNNANDE             
006400*                                 CODE FOR APPROVAL OF SCHEDULE P         
006500*                                 ROPOSAL                                 
006600     03 UPLP-KDLPORS-TAB     OCCURS 3 TIMES                               
006700                             PIC S9(3)           COMP-3.                  
006800*                                 LEVERANSPLANEORSAK                      
006900     03 UPLP-KDLPSP          PIC S9              COMP-3.                  
007000*                                 LEVERANSPLANESPƒRR                      
007100     03 UPLP-KDPLKOEP        PIC S9              COMP-3.                  
007200*                                 STATUS AVTALSK÷P (PLAN)                 
007300*                                 1=F÷RESLAGEN  2=GODKƒND                 
007400     03 UPLP-KVAVROP         PIC S9(7)           COMP-3.                  
007500*                                 AVROPSKVANTITET                         
007600     03 UPLP-KVBEST-PL       PIC S9(7)           COMP-3.                  
007700*                                 BESTƒLLNINGSKVANTITET P≈ PLAN           
007800     03 UPLP-TIAVRDAT-DISP   PIC S9(7)           COMP-3.                  
007900*                                 PLANERAT DISPONIBLEDATUM                
008000     03 UPLP-TIAVRDAT-INL    PIC S9(7)           COMP-3.                  
008100*                                 PLANERAT INLEVERANSDATUM                
008200     03 UPLP-TILEVDAG        PIC S9              COMP-3.                  
008300*                                 AVSƒNDNINGSDAG INOM VECKA               
008400*                                 DELIVERY WEEK DAY                       
008500     03 UPLP-TILPSP          PIC S9(5)           COMP-3.                  
008600*                                 DATUM LEVERANSPLAN-SPƒRR (≈≈VV)         
008700     03 UPLP-TIOMSPEC        PIC S9(5)           COMP-3.                  
008800*                                 OMSPECIFIKATIONSDATUM  (≈≈VV)           
008900     03 UPLP-FLSKROT-WLC     PIC X.                                       
009000*                                 SISTA AVROP F÷RE SKROT                  
009100*                                 LAST CALL BEFORE SCRAPPING              
009200     03 UPLP-IDLEVNR-SHIP    PIC X(5).                                    
009300*                                 SKEPPANDE LEVERANT÷R                    
009400*                                 SHIPPING SUPPLIER                       
009500     03 UPLP-IDLANDX2-SHIP   PIC X(2).                                    
009600*                                 2-STƒLLIG LANDSBETECKNINGSKOD           
009700*                                 2-LETTER CODE FOR COUNTRY               
009800     03 UPLP-IDANSK          PIC S9(3)           COMP-3.                  
009900*                                 ANSKAFFARNUMMER                         
010000*                                 PROCURER NO.                            
010100     03 UPLP-KVQ             PIC S9(7)           COMP-3.                  
010200*                                 EKONOMISK HEMTAGNINGSKVANTITET          
010300     03 UPLP-KVPALL          PIC S9(7)           COMP-3.                  
010400*                                 ANTAL I PALL                            
010500*                                 QUANTITY IN PALLET                      
010600     03 UPLP-KVULOAD         PIC S9(7)           COMP-3.                  
010700*                                 MIN ENHETSLAST FR≈N LEVERANT÷R          
010800*                                 MIN LOAD FROM SUPPLIER                  
010900     03 UPLP-TIAAMMDD-SPECST PIC S9(7)           COMP-3.                  
011000*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
011100*                                 YEAR - MONTH - DAY  (YYMMDD)            
011200     03 UPLP-TIAAMMDD-FT     PIC S9(7)           COMP-3.                  
011300*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
011400*                                 YEAR - MONTH - DAY  (YYMMDD)            
011500     03 UPLP-KVDAGAR-TT      PIC S9(3)           COMP-3.                  
011600*                                 DAGAR TULL- OCH TRANSPORT-TID           
011700     03 UPLP-KVDAGAR-INLEV   PIC S9(3)           COMP-3.                  
011800*                                 INLEVERANSTID     (ANTAL DAGAR)         
011900     03 UPLP-FLAGGA-DAGL-AVROP                                            
012000                             PIC X.                                       
012100*                                 ALLMƒN FLAGGA                           
012200*                                 GENERAL FLAG                            
012300     03 UPLP-TILEVDAG-DAGL   OCCURS 5 TIMES                               
012400                             PIC S9              COMP-3.                  
012500*                                 AVSƒNDNINGSDAG INOM VECKA               
012600*                                 DELIVERY WEEK DAY                       
012700     03 UPLP-DAAVROP-FOM     PIC 9(6).                                    
012800*                                 STARTVECKA ≈TGƒRD AVROP                 
012900*                                 (≈≈≈≈VV)                                
013000*                                 START WEEK ACTION AVROP                 
013100*                                 (≈≈≈≈VV)                                
013200     03 UPLP-DAAVROP-TOM     PIC 9(6).                                    
013300*                                 SLUTVECKA ≈TGƒRD AVROP                  
013400*                                 (≈≈≈≈VV)                                
013500*                                 END WEEK ACTION AVROP                   
013600*                                 (≈≈≈≈VV)                                
013700     03 UPLP-DAAVROP-TFOM    PIC 9(6).                                    
013800*                                 TIDIGARELAGD STARTVECKA AVROP           
013900*                                 (≈≈≈≈VV)                                
014000*                                 START WEEK MOVED AVROP                  
014100*                                 (≈≈≈≈VV)                                
014200*** END OF VILMAII-COPY LENGTH= 118 BYTES                                 
