000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.             W411AREG.                                        
000500 AUTHOR.                 LARS THELL CAP GEMINI LOGIC                      
000600     DATE-WRITTEN.       MAJ 1990.                                        
000700*                                                                         
000800     REMARKS.                                                             
000900*                                                                         
001000*    HELT OMSKRIVET AV STEFANO GIOBBI FÖR SDC-PROJEKTET I NOV 94.         
001100*                                                                         
001200*    FUNKTION:                                                            
001300*        PROGRAMMET HÄMTAR ARTIKELINFORMATION FÖR ANGIVET                 
001400*        ARTIKELNR                                                        
001500*                                                                         
001600*        KONTROLL AV ARTIKELN HOS VCAS                                    
001700*    STORY 2217565 : RECOMPILING FOR WWDCLAND,WWDC99                      
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200                                                                          
002300 WORKING-STORAGE SECTION.                                                 
002400     SKIP2                                                                
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  PROGRAM-NAMN            PIC X(8) VALUE 'W411AREG'.                   
002800     SKIP2                                                                
002900*    ---- KONSTANTER                                                      
003000                                                                          
003100 77  JA                      PIC X       VALUE 'J'.                       
003200 77  NEJ                     PIC X       VALUE 'N'.                       
003300*      --- VALID IDDC CODES                                               
003400*                                                                         
003500*01    -COPY WWDC99                                                       
003510*01    -COPY WWDCLAND                                                     
003600       EJECT                                                              
003700     EJECT                                                                
003800 01  DYNAMISKA-SUBPROGRAM.                                                
003900   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
004000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
004100     SKIP2                                                                
004200*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
004300                                                                          
004400 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
004500     SKIP2                                                                
004600*    ---- STATUSKOD FRÅN IMS                                              
004700                                                                          
004800 01  STATUS-WS               PIC XX.                                      
004900     88  SEGMENT-FINNS                    VALUE '  '.                     
005000     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
005100     88  SEGMENT-SLUT                     VALUE 'GB'.                     
005200     SKIP2                                                                
005300 01  GODK-STATUSKODER.                                                    
005400   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
005500     SKIP2                                                                
005600 01  SSA1                    PIC X(64).                                   
005700 01  SSA2                    PIC X(64).                                   
005710 01  SSA3                    PIC X(64).                                   
005800     SKIP2                                                                
005900*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
006000                                                                          
006100 01  NYCKLAR-TILL-DLI.                                                    
006200                                                                          
006300   03  W-IDARTNR-X.                                                       
006400     05  W-IDARTNR           PIC S9(9) COMP-3.                            
006500                                                                          
006600   03  W-IDDC-X.                                                          
006700     05  W-IDDC              PIC X(2).                                    
006800                                                                          
006801   03  W-IDLAND-X.                                                        
006802     05  W-IDLAND            PIC X(2).                                    
006803                                                                          
006810   03  W-KDSEGKEY-X.                                                      
006820     05  W-KDSEGKEY          PIC X(1)  VALUE '1'.                         
006830                                                                          
006900                                                                          
007000   03  W-IDDC-REC-X.                                                      
007100     05  W-IDDC-REC          PIC X(2).                                    
007200                                                                          
007300                                                                          
007400                                                                          
007500     EJECT                                                                
007600*01  -COPY W0003                                                          
007700     EJECT                                                                
007800 01  FILLER                  PIC X(16) VALUE 'IO-AREA-K601'.              
007900 01  IO-AREA-K601.                                                        
008000*    03  -COPY WDK601                                                     
008100     EJECT                                                                
008200                                                                          
008300 01  FILLER                  PIC X(16) VALUE 'IO-AREA-K611'.              
008400 01  IO-AREA-K611.                                                        
008500*    03  -COPY WDK611                                                     
008600     EJECT                                                                
008700                                                                          
008800 01  FILLER                  PIC X(16) VALUE 'IO-AREA-K711'.              
008900 01  IO-AREA-K711.                                                        
009000*    03  -COPY WDK711                                                     
009100     EJECT                                                                
009101                                                                          
009120 01  FILLER                  PIC X(16) VALUE 'IO-AREA-K722'.              
009130 01  IO-AREA-K722.                                                        
009140*    03  -COPY WDK722                                                     
009150     EJECT                                                                
009200 LINKAGE SECTION.                                                         
009300     SKIP2                                                                
009400*01  -COPY W411AREG                                                       
009500     EJECT                                                                
009600*01  -COPY W0008      -PRE  WDK6-                                         
009700       05  FILLER                PIC X.                                   
009800*01  -COPY W0008      -PRE  WDK7-                                         
009900       05  FILLER                PIC X.                                   
010000     EJECT                                                                
010100 PROCEDURE DIVISION  USING  AREG-W411AREG                                 
010200                            WDK6-PCB                                      
010300                            WDK7-PCB.                                     
010400                                                                          
010500 STYR SECTION.                                                            
010600                                                                          
010700     PERFORM A-INIT                                                       
010800                                                                          
010900     PERFORM B-LAS-ARTIKEL                                                
011000                                                                          
011100     GOBACK                                                               
011200     .                                                                    
011300     EJECT                                                                
011400 A-INIT        SECTION.                                                   
011500                                                                          
011600     MOVE +0    TO AREG-KDORDBEK                                          
011700                   AREG-ADGANG                                            
011800                   AREG-ADLAGOMR                                          
011900                   AREG-ADPLATS                                           
012300                   AREG-IDANSK                                            
012400                   AREG-IDFKNGRP                                          
012500                   AREG-IDLKTO                                            
012600                   AREG-IDPSN                                             
012900                   AREG-KDARTURS                                          
013000                   AREG-KDERS                                             
013100                   AREG-KDERS-UTG                                         
013200                   AREG-KDFARLIG                                          
013300                   AREG-KDLEVSP                                           
013400                   AREG-KDPRODSL                                          
013500                   AREG-KDSPEEMB                                          
013500                   AREG-KDVSOP                                            
013600                   AREG-KDVVKL                                            
013700                   AREG-KVAKS-CDC                                         
013800                   AREG-KVAKS-PAV                                         
013900                   AREG-KVFRYSTI                                          
014000                   AREG-KVLS                                              
014201                   AREG-KVPB-SATS                                         
014301                   AREG-KVPB-SEP                                          
014401                   AREG-KVPB-TPO                                          
014501                   AREG-KVQPACK-0                                         
014502                   AREG-KVQPACK-1                                         
014601                   AREG-KVQPACK-3                                         
014701                   AREG-KVQPACK-4                                         
014801                   AREG-KVRESS                                            
014901                   AREG-KVROS                                             
015001                   AREG-KVSLUTKP                                          
015101                   AREG-KVSPANT                                           
015201                   AREG-KVUTRS                                            
015301                   AREG-KVSPARR-KVAL                                      
015501                   AREG-PRARTSTD                                          
015601                   AREG-REDIRLEV                                          
015701                   AREG-REKSIFFR                                          
015801                   AREG-TIDISPIN                                          
015901                   AREG-TIFINLV                                           
016001                   AREG-VKART                                             
016002                   AREG-VKART-NTO                                         
016101                   AREG-VLARTNTO                                          
016201     MOVE SPACE TO                                                        
016301                   AREG-IDLEVNR                                           
016401                   AREG-KDSORT                                            
016501                   AREG-FLIART                                            
016601                   AREG-FLAVRART                                          
016701                   AREG-FLLSRDEL                                          
016801                   AREG-FLMARKSP                                          
016901                   AREG-FLRADREF                                          
017001                   AREG-FLREFILL                                          
017101                   AREG-FLTPO1                                            
017201                   AREG-KDUART                                            
017301                   AREG-FLRELSP                                           
017401                   AREG-FLCDCBEH                                          
017501                   AREG-IDUSER-SPKVAL                                     
017601     .                                                                    
017701     EJECT                                                                
017801 B-LAS-ARTIKEL SECTION.                                                   
017901                                                                          
018001     MOVE AREG-IDARTNR          TO W-IDARTNR                              
018101     MOVE AREG-IDDC             TO W-IDDC                                 
018201                                   WS-IDDC                                
018301     MOVE AREG-IDDC-REC         TO W-IDDC-REC                             
018401                                                                          
018501     PERFORM IMS-GU-WDK601                                                
018601     IF SEGMENT-FINNS                                                     
018701       IF ART-KDERS-UTG = +0                                              
018801         PERFORM BA-LAGRA-ARTIKELINFORMATION                              
018901         IF AREG-FLRELSP = 'J'                                            
019001           MOVE 56 TO AREG-KDORDBEK                                       
019101         END-IF                                                           
019201                                                                          
019301         IF AREG-IDDC-REC NOT = SPACE                                     
019401           PERFORM IMS-GU-WDK711-REC                                      
019501           IF SEGMENT-FINNS                                               
019601             MOVE SLAG-FLCDCBEH  TO AREG-FLCDCBEH                         
019701           END-IF                                                         
019801         END-IF                                                           
019901                                                                          
020001         IF NDC-NA                                                        
020101           PERFORM IMS-GU-WDK711                                          
020201           IF SEGMENT-FINNS                                               
020203             MOVE SLAG-ADLAGOMR TO AREG-ADLAGOMR                          
020901           END-IF                                                         
021001         END-IF                                                           
021101       ELSE                                                               
021201         MOVE ART-KDERS-UTG     TO AREG-KDERS-UTG                         
021301         MOVE ART-KDPRODSL      TO AREG-KDPRODSL                          
021401       END-IF                                                             
021501     ELSE                                                                 
021601       MOVE 58 TO AREG-KDORDBEK                                           
021701     END-IF                                                               
021801     .                                                                    
021901     EJECT                                                                
022001 BA-LAGRA-ARTIKELINFORMATION  SECTION.                                    
022101                                                                          
022201     PERFORM BAA-LAGRA-WDK601-INFO                                        
022301                                                                          
022401     PERFORM IMS-GNP-WDK611                                               
022501     PERFORM BAB-LAGRA-WDK611-INFO                                        
022502                                                                          
022503     IF CDC-SE OR AREG-IDDC = SPACE                                       
022504        CONTINUE                                                          
022505     ELSE                                                                 
022506        PERFORM BAC-LAGRA-MULTIPEL-INFO                                   
022507     END-IF                                                               
022601     .                                                                    
022701     EJECT                                                                
022801 BAA-LAGRA-WDK601-INFO SECTION.                                           
022901                                                                          
023001     MOVE ART-FLIART           TO AREG-FLIART                             
023101     MOVE ART-IDFKNGRP         TO AREG-IDFKNGRP                           
023201     MOVE ART-IDLEVNR          TO AREG-IDLEVNR                            
023301     MOVE ART-KDERS-UTG        TO AREG-KDERS-UTG                          
023401     MOVE ART-KDPRODSL         TO AREG-KDPRODSL                           
023501     MOVE ART-KDSORT           TO AREG-KDSORT                             
023601     MOVE ART-REKSIFFR         TO AREG-REKSIFFR                           
023701     MOVE ART-TIFINLV          TO AREG-TIFINLV                            
023801     .                                                                    
023901     EJECT                                                                
024001 BAB-LAGRA-WDK611-INFO SECTION.                                           
024101                                                                          
024201     MOVE CLAG-ADART           TO AREG-ADART                              
024401     MOVE CLAG-FLAVRART        TO AREG-FLAVRART                           
024501     MOVE CLAG-FLLSRDEL        TO AREG-FLLSRDEL                           
024601     MOVE CLAG-FLMARKSP        TO AREG-FLMARKSP                           
024701     MOVE CLAG-FLRADREF        TO AREG-FLRADREF                           
024801     MOVE CLAG-FLREFILL        TO AREG-FLREFILL                           
024901     MOVE CLAG-FLTPO1          TO AREG-FLTPO1                             
025001     MOVE CLAG-IDANSK          TO AREG-IDANSK                             
025101     MOVE CLAG-IDLKTO          TO AREG-IDLKTO                             
025201     MOVE CLAG-IDPSN           TO AREG-IDPSN                              
025401     MOVE CLAG-KDARTURS        TO AREG-KDARTURS                           
025501     MOVE CLAG-KDERS           TO AREG-KDERS                              
025601     MOVE CLAG-KDFARLIG        TO AREG-KDFARLIG                           
025701     MOVE CLAG-KDLEVSP         TO AREG-KDLEVSP                            
025801     MOVE CLAG-KDSPEEMB        TO AREG-KDSPEEMB                           
025901     MOVE CLAG-KDUART          TO AREG-KDUART                             
026001     MOVE CLAG-KDVVKL          TO AREG-KDVVKL                             
026101     MOVE CLAG-KVAKS-CDC       TO AREG-KVAKS-CDC                          
026201     MOVE CLAG-KVAKS-PAV       TO AREG-KVAKS-PAV                          
026301     MOVE CLAG-KVFRYSTI        TO AREG-KVFRYSTI                           
026401     MOVE CLAG-KVLS            TO AREG-KVLS                               
026501     MOVE CLAG-KVLS-SVS        TO AREG-KVLS-SVS                           
026501     MOVE CLAG-KDVSOP          TO AREG-KDVSOP                             
026701     MOVE CLAG-KVPB-SATS       TO AREG-KVPB-SATS                          
026801     MOVE CLAG-KVPB-SEP        TO AREG-KVPB-SEP                           
026901     MOVE CLAG-KVPB-TPO        TO AREG-KVPB-TPO                           
027001     MOVE CLAG-KVQPACK-0       TO AREG-KVQPACK-0                          
027002     MOVE CLAG-KVQPACK-1       TO AREG-KVQPACK-1                          
027101     MOVE CLAG-KVQPACK-3       TO AREG-KVQPACK-3                          
027201     MOVE CLAG-KVQPACK-4       TO AREG-KVQPACK-4                          
027301     MOVE CLAG-KVRESS          TO AREG-KVRESS                             
027401     MOVE CLAG-KVROS           TO AREG-KVROS                              
027501     MOVE CLAG-KVSLUTKP        TO AREG-KVSLUTKP                           
027601     MOVE CLAG-KVSPANT         TO AREG-KVSPANT                            
027701     MOVE CLAG-KVUTRS          TO AREG-KVUTRS                             
027801     MOVE CLAG-KVSPARR-KVAL    TO AREG-KVSPARR-KVAL                       
028001     MOVE CLAG-PRARTSTD        TO AREG-PRARTSTD                           
028101     MOVE CLAG-REDIRLEV        TO AREG-REDIRLEV                           
028201     MOVE CLAG-TIDISPIN        TO AREG-TIDISPIN                           
028301     MOVE CLAG-VKART           TO AREG-VKART                              
028302     MOVE CLAG-VKART-NTO       TO AREG-VKART-NTO                          
028401     MOVE CLAG-VLARTNTO        TO AREG-VLARTNTO                           
028501     MOVE CLAG-FLRELSP         TO AREG-FLRELSP                            
028601     MOVE CLAG-IDUSER-SPKVAL   TO AREG-IDUSER-SPKVAL                      
028701     MOVE SPACE                TO AREG-FLCCC                              
028801     .                                                                    
028901     EJECT                                                                
028902 BAC-LAGRA-MULTIPEL-INFO SECTION.                                         
028903                                                                          
028904     SEARCH ALL DC-LAND                                                   
028905        AT END                                                            
028906           MOVE SPACE          TO W-IDLAND                                
028907        WHEN DCLAND-IDDC (DCLAND-IX) = AREG-IDDC                          
028908           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
028909                               TO W-IDLAND                                
028910     END-SEARCH                                                           
028911                                                                          
028912     PERFORM IMS-GU-WDK722                                                
028913     IF SEGMENT-FINNS                                                     
028914        IF XLAG-IDANSK > 0                                                
028915          MOVE XLAG-IDANSK     TO AREG-IDANSK                             
028916        END-IF                                                            
028917        MOVE XLAG-KVSLUTKP     TO AREG-KVSLUTKP                           
028918        MOVE XLAG-KVSPANT      TO AREG-KVSPANT                            
028919     ELSE                                                                 
028920        IF DCLAND-CHINA (DCLAND-IX)                                       
028921           MOVE ZERO           TO AREG-KVSLUTKP                           
028923        END-IF                                                            
028924        MOVE ZERO              TO AREG-KVSPANT                            
028925     END-IF                                                               
028947     .                                                                    
028950     EJECT                                                                
029001 IMS-GU-WDK601      SECTION.                                              
029101                                                                          
029201     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
029301            DELIMITED BY SIZE INTO    SSA1                                
029401     MOVE    '  GE'             TO    GODK-STATUSKODER                    
029501     CALL    CBLTDLI            USING GU           WDK6-PCB               
029601                                      IO-AREA-K601 SSA1                   
029701     MOVE    WDK6-STATUS-CODE   TO    STATUS-WS                           
029801     PERFORM IMS-STATUSKONTROLL                                           
029901     .                                                                    
030001     SKIP2                                                                
030101 IMS-GNP-WDK611     SECTION.                                              
030201                                                                          
030301     MOVE    'WDK611   '        TO    SSA1                                
030401     MOVE    '  '               TO    GODK-STATUSKODER                    
030501     CALL    CBLTDLI            USING GNP          WDK6-PCB               
030601                                      IO-AREA-K611 SSA1                   
030701     MOVE    WDK6-STATUS-CODE   TO    STATUS-WS                           
030801     PERFORM IMS-STATUSKONTROLL                                           
030901     .                                                                    
031001     EJECT                                                                
031101 IMS-GU-WDK711         SECTION.                                           
031201                                                                          
031301     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
031401          DELIMITED BY SIZE INTO SSA1                                     
031501     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
031601          DELIMITED BY SIZE INTO SSA2                                     
031701     MOVE '  GE'                TO GODK-STATUSKODER                       
031801     CALL CBLTDLI USING GU                                                
031901                        WDK7-PCB                                          
032001                        IO-AREA-K711                                      
032101                        SSA1                                              
032201                        SSA2                                              
032301     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
032401     PERFORM IMS-STATUSKONTROLL                                           
032501     .                                                                    
032601                                                                          
032618 IMS-GU-WDK722         SECTION.                                           
032619                                                                          
032620     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
032621          DELIMITED BY SIZE INTO SSA1                                     
032622     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
032623          DELIMITED BY SIZE INTO SSA2                                     
032624     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032625          DELIMITED BY SIZE INTO SSA3                                     
032626     MOVE '  GE'                TO GODK-STATUSKODER                       
032627     CALL CBLTDLI USING GU                                                
032628                        WDK7-PCB                                          
032629                        IO-AREA-K722                                      
032630                        SSA1                                              
032640                        SSA2                                              
032641                        SSA3                                              
032650     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
032660     PERFORM IMS-STATUSKONTROLL                                           
032670     .                                                                    
032680                                                                          
032701 IMS-GU-WDK711-REC    SECTION.                                            
032801                                                                          
032901     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
033001          DELIMITED BY SIZE INTO SSA1                                     
033101     STRING 'WDK711  (IDDC     =' W-IDDC-REC-X ')'                        
033201          DELIMITED BY SIZE INTO SSA2                                     
033301     MOVE '  GE'                TO GODK-STATUSKODER                       
033401     CALL CBLTDLI USING GU                                                
033501                        WDK7-PCB                                          
033601                        IO-AREA-K711                                      
033701                        SSA1                                              
033801                        SSA2                                              
033901     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
034001     PERFORM IMS-STATUSKONTROLL                                           
034101     .                                                                    
034201     SKIP3                                                                
034301 IMS-STATUSKONTROLL SECTION.                                              
034401                                                                          
034501     SET STATUS-IX TO 1                                                   
034601     SEARCH GODK-STATUS                                                   
034701       AT END CALL FELLOG                                                 
034801       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
034901     END-SEARCH                                                           
035001     .                                                                    
