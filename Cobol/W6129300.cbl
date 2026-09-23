001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6129300.                                                
001400 AUTHOR.         NIHLBLAD JOHAN.                                          
001500 DATE-WRITTEN.   02/04/02.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PRINTAR FÖR PGM W6031700                                         
002100*                                                                         
002201*        PROGRAMMET LÄSER      WDD8                                       
002202*        PROGRAMMET LÄSER      WDK7                                       
002210*        PROGRAMMET LÄSER      WDJ8                                       
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W6129300'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400     SKIP2                                                                
004410*    --- INDEX FOR LINES                                                  
004420 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004430 77  MAX-INDX                    PIC S9(4)  VALUE +31   COMP SYNC.        
004440 77  SID-INDX                    PIC 999    VALUE ZERO.                   
004450*    --- INDEX FOR IDARTNR                                                
004460 77  TAB-INDX                    PIC 99     VALUE ZERO.                   
004470 77  TAB-MAX-INDX                PIC 99     VALUE ZERO.                   
004471*                                                                         
004472 01  WORK-IDARTNR-TAB-GRP.                                                
004473     03 WORK-IDARTNR-TAB     PIC X(9)                                     
004474                             OCCURS 99.                                   
004480*    --- WORK FIELDS FOR ACTUAL KEYVALUES.                                
004490                                                                          
004506 01  PARM-SYSIN.                                                          
004507     03 PARM-IDDC                PIC X(2)   VALUE ZERO.                   
004508     03 PARM-ADLAGOMR            PIC 9(2)   VALUE ZERO.                   
004509     03 PARM-ADGANG-FOM          PIC 9(2)   VALUE ZERO.                   
004510     03 PARM-ADGANG-TOM          PIC 9(2)   VALUE ZERO.                   
004511     03 PARM-ADSEC-FOM           PIC 9(2)   VALUE ZERO.                   
004512     03 PARM-ADSEC-TOM           PIC 9(2)   VALUE ZERO.                   
004514     03 PARM-ADLEVEL-FOM         PIC 9(2)   VALUE ZERO.                   
004515     03 PARM-ADLEVEL-TOM         PIC 9(2)   VALUE ZERO.                   
004518     03 PARM-KDLOC               PIC X(1)   VALUE SPACE.                  
004519     03 PARM-KDFREQ              PIC 9(2)   VALUE ZERO.                   
004520     03 PARM-TELOC               PIC X(1)   VALUE SPACE.                  
004521     03 PARM-KDPRT               PIC X(3)   VALUE SPACE.                  
004522     03 FILLER                   PIC X(57)  VALUE SPACE.                  
004523*                                                                         
004524                                                                          
004525 77  PRIME                       PIC X        VALUE 'P'.                  
004526 77  BUFFER                      PIC X        VALUE 'R'.                  
004527 77  MIXED                       PIC X        VALUE 'M'.                  
004528                                                                          
004529 77 ACK-ANTAL-RADER              PIC 9(3)     VALUE ZERO.                 
004530 77 ACK-ANTAL-SIDOR              PIC 9(3)     VALUE ZERO.                 
004531                                                                          
004540 01  FELTEXT.                                                             
004600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200     EJECT                                                                
005300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005400 01  FILLER REDEFINES DAGENS-DATUM.                                       
005500     03  AAR                     PIC 9(2).                                
005600     03  MAN                     PIC 9(2).                                
005700     03  DAG                     PIC 9(2).                                
005800     EJECT                                                                
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000*                                                                         
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  W980SOP                 PIC X(8)    VALUE 'WW980SOP'.            
006400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006510     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL WW980SOP                                         
006800     SKIP2                                                                
006900 01  FILLER                      PIC X(16)   VALUE                        
007000                                             'W980SOP-AREA'.              
007100     SKIP2                                                                
007200*01  -COPY WSOPAREA                                                       
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL DATKORT                                          
007500*                                                                         
007600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61293'.              
007700     SKIP2                                                                
007800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007900     SKIP2                                                                
008000*01  -COPY WDATKORT                                                       
008301     EJECT                                                                
008302* VARIABLER TILL SUBPROGRAM W006PRS1                                      
008303     EJECT                                                                
008304*01  -COPY W006PRAR                                                       
008305     SKIP2                                                                
008306 01  W-IDPRTLST                  PIC X(8).                                
008307     SKIP2                                                                
008310 01  RAD                         PIC X(132).                              
008500*                                                                         
008510 01  TEXT-RADER.                                                          
008520*                                                                         
008530     03  RUBRIKRAD-1.                                                     
008540        05  FILLER                PIC X(19) VALUE                         
008550           'LIST LOCATIONS     '.                                         
008560        05  FILLER                PIC X(12) VALUE                         
008570           'LIST 6129300'.                                                
008580        05  FILLER                PIC X(60) VALUE SPACE.                  
008590        05  FILLER                PIC X(5)  VALUE 'DATE:'.                
008591        05  LIST-AAR              PIC 9(2).                               
008592        05  FILLER                PIC X(1)  VALUE '-'.                    
008593        05  LIST-MAN              PIC 9(2).                               
008594        05  FILLER                PIC X(1)  VALUE '-'.                    
008595        05  LIST-DAG              PIC 9(2).                               
008596        05  FILLER                PIC X(7)  VALUE '  PAGE:'.              
008597        05  LIST-PAGE             PIC Z(3)9 VALUE ZERO.                   
008598        05  FILLER                PIC X(17) VALUE SPACE.                  
008599*                                                                         
008600     03  RUBRIKRAD-2.                                                     
008601        05  FILLER                PIC X(132) VALUE SPACE.                 
008602*                                                                         
008603     03  RUBRIKRAD-3.                                                     
008604        05  FILLER                PIC X(3)  VALUE 'DC:'.                  
008605        05  LIST-DC               PIC X(2).                               
008606        05  FILLER                PIC X(7)  VALUE '  AREA:'.              
008607        05  LIST-AREA             PIC X(2).                               
008608        05  FILLER                PIC X(5)  VALUE '  AF:'.                
008609        05  LIST-AF               PIC X(2).                               
008610        05  FILLER                PIC X(5)  VALUE '  AT:'.                
008611        05  LIST-AT               PIC X(2).                               
008612        05  FILLER                PIC X(5)  VALUE '  SF:'.                
008613        05  LIST-SF               PIC X(2).                               
008614        05  FILLER                PIC X(5)  VALUE '  ST:'.                
008615        05  LIST-ST               PIC X(2).                               
008616        05  FILLER                PIC X(5)  VALUE '  LF:'.                
008617        05  LIST-LF               PIC X(2).                               
008618        05  FILLER                PIC X(5)  VALUE '  LT:'.                
008619        05  LIST-LT               PIC X(2).                               
008620        05  FILLER                PIC X(7)  VALUE '  TYPE:'.              
008621        05  LIST-TYPE             PIC X(1).                               
008622        05  FILLER                PIC X(7)  VALUE '  FREQ:'.              
008623        05  LIST-FREQ             PIC X(2).                               
008624        05  FILLER                PIC X(7)  VALUE '  SPEC:'.              
008625        05  LIST-SPEC             PIC X(1).                               
008626        05  FILLER                PIC X(56) VALUE SPACE.                  
008627*                                                                         
008628     03  RUBRIKRAD-4.                                                     
008629        05  FILLER                PIC X(132) VALUE SPACE.                 
008630*                                                                         
008631     03  RUBRIKRAD-5.                                                     
008632        05  FILLER              PIC X(8)   VALUE 'LOCATION'.              
008633        05  FILLER              PIC X(9)   VALUE '     TYPE'.             
008634        05  FILLER              PIC X(6)   VALUE '  FREQ'.                
008635        05  FILLER              PIC X(11)  VALUE '  STORAGE  '.           
008636        05  FILLER              PIC X(14)  VALUE 'SPEC.DESCRIPT.'.        
008637        05  FILLER              PIC X(14)  VALUE '   PART NUMBER'.        
008638        05  FILLER              PIC X(70)  VALUE SPACE.                   
008639*                                                                         
008640     03  DETALJRAD.                                                       
008641        05  LIST-ADLAGOMR-LINE    PIC X(2).                               
008642        05  FILLER                PIC X      VALUE SPACE.                 
008643        05  LIST-ADGANG-LINE      PIC X(2).                               
008644        05  FILLER                PIC X      VALUE SPACE.                 
008645        05  LIST-ADPLATS-LINE     PIC X(5).                               
008646        05  FILLER                PIC X(3)   VALUE SPACE.                 
008647        05  LIST-KDLOC-LINE       PIC X(1).                               
008648        05  FILLER                PIC X(5)   VALUE SPACE.                 
008649        05  LIST-KDFREQ-LINE      PIC X(2).                               
008650        05  FILLER                PIC X(4)   VALUE SPACE.                 
008651        05  LIST-KDSTOR-LINE      PIC X(3).                               
008652        05  FILLER                PIC X(5)   VALUE SPACE.                 
008653        05  LIST-TELOC-LINE       PIC X(15).                              
008654        05  FILLER                PIC X(3)   VALUE SPACE.                 
008655        05  LIST-IDARTNR-LINE     PIC X(9).                               
008656        05  FILLER                PIC X(71)  VALUE SPACE.                 
008657*                                                                         
008660     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900*    ----WORK-AREAS FOR IMS-SECTIONS                                      
009000*                                                                         
009010     EJECT                                                                
009011 01  WORK-AREA.                                                           
009012     03 WORK-ADLAGOMR            PIC 9(2).                                
009013     03 WORK-ADGANG              PIC 9(2).                                
009014     03 WORK-ADPLATS.                                                     
009015        05 WORK-ADSEC            PIC 9(2).                                
009016        05 WORK-ADLEVEL          PIC 9(2).                                
009017        05 WORK-ADSEQ            PIC 9(1).                                
009018     03 WORK-TELOC               PIC X(1).                                
009019     03 WORK-IDARTNR             PIC X(9).                                
009020   03  WDD8ASEQ-WORK.                                                     
009021     05  W-IDDC-ASEQ-WORK        PIC X(2)           VALUE SPACE.          
009022     05  W-ADBUFFOMR-ASEQ-WORK   PIC S9(3)  COMP-3  VALUE ZERO.           
009023     05  W-ADBUFFGANG-ASEQ-WORK  PIC S9(3)  COMP-3  VALUE ZERO.           
009024     05  W-ADBUFFPL-ASEQ-WORK    PIC S9(5)  COMP-3  VALUE ZERO.           
009025     05  W-DABUFPAF-ASEQ-WORK    PIC  9(8)  COMP-3  VALUE ZERO.           
009026     05  W-IDARTNR-ASEQ-WORK     PIC S9(9)  COMP-3  VALUE ZERO.           
009027 01  KEYS-TO-DLI.                                                         
009030                                                                          
009040     03  W-WDJ8KY-MIN-X.                                                  
009050         05  W-LOC-IDDC-MIN       PIC X(2).                               
009060         05  W-LOC-ADLAGOMR-MIN   PIC 9(2).                               
009070         05  W-LOC-ADGANG-MIN     PIC 9(2).                               
009080         05  W-LOC-ADPLATS-MIN.                                           
009090             07 W-LOC-ADSEC-MIN   PIC 9(2).                               
009091             07 W-LOC-ADLEVEL-MIN PIC 9(2).                               
009092             07 W-LOC-ADSEQ-MIN   PIC 9(1).                               
009093                                                                          
009094     03  W-WDJ8KY-MAX-X.                                                  
009095         05  W-LOC-IDDC-MAX       PIC X(2).                               
009096         05  W-LOC-ADLAGOMR-MAX   PIC 9(2).                               
009097         05  W-LOC-ADGANG-MAX     PIC 9(2).                               
009098         05  W-LOC-ADPLATS-MAX.                                           
009099             07 W-LOC-ADSEC-MAX   PIC 9(2).                               
009100             07 W-LOC-ADLEVEL-MAX PIC 9(2).                               
009101             07 W-LOC-ADSEQ-MAX   PIC 9(1).                               
009102                                                                          
009103     03  W-WDJ8KEY-X.                                                     
009104         05  W-LOC-IDDC          PIC X(2)    VALUE SPACE.                 
009105         05  W-LOC-ADLAGOMR      PIC 9(2)    VALUE ZERO.                  
009106         05  W-LOC-ADGANG        PIC 9(2)    VALUE ZERO.                  
009107         05  W-LOC-ADPLATS.                                               
009108             07 W-LOC-ADSEC      PIC 9(2).                                
009109             07 W-LOC-ADLEVEL    PIC 9(2).                                
009110             07 W-LOC-ADSEQ      PIC 9(1).                                
009111                                                                          
009112   03  WDK7A1KY-MIN-X.                                                    
009113     05  W-IDDC-MIN          PIC X(2)           VALUE SPACE.              
009114     05  W-ADART-MIN.                                                     
009115       07  W-ADLAGOMR-MIN    PIC S9(3)  COMP-3  VALUE ZERO.               
009116       07  W-ADGANG-MIN      PIC S9(3)  COMP-3  VALUE ZERO.               
009117       07  W-ADPLATS-MIN     PIC S9(5)  COMP-3  VALUE ZERO.               
009118     05 W-IDARTNR-MIN        PIC S9(9)  COMP-3  VALUE ZERO.               
009119                                                                          
009120   03  WDK7A1KY-MAX-X.                                                    
009121     05  W-IDDC-MAX          PIC X(2)           VALUE SPACE.              
009122     05  W-ADART-MAX.                                                     
009123       07  W-ADLAGOMR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.               
009124       07  W-ADGANG-MAX      PIC S9(3)  COMP-3  VALUE ZERO.               
009125       07  W-ADPLATS-MAX     PIC S9(5)  COMP-3  VALUE ZERO.               
009126     05 W-IDARTNR-MAX     PIC S9(9)  COMP-3  VALUE 99999999.              
009127                                                                          
009128   03  W-WDD8ASEQ-X.                                                      
009129     05  W-IDDC-ASEQ         PIC X(2)           VALUE SPACE.              
009130     05  W-ADBUFFOMR-ASEQ    PIC S9(3)  COMP-3  VALUE ZERO.               
009131     05  W-ADBUFFGANG-ASEQ   PIC S9(3)  COMP-3  VALUE ZERO.               
009132     05  W-ADBUFFPL-ASEQ     PIC S9(5)  COMP-3  VALUE ZERO.               
009133                                                                          
009134   03  WDD8ASEQ-MIN-X.                                                    
009135     05  W-IDDC-ASEQ-MIN         PIC X(2)           VALUE SPACE.          
009136     05  W-ADBUFFOMR-ASEQ-MIN    PIC S9(3)  COMP-3  VALUE ZERO.           
009137     05  W-ADBUFFGANG-ASEQ-MIN   PIC S9(3)  COMP-3  VALUE ZERO.           
009138     05  W-ADBUFFPL-ASEQ-MIN     PIC S9(5)  COMP-3  VALUE ZERO.           
009139     05  W-DABUFPAF-ASEQ-MIN     PIC  9(8)  COMP-3  VALUE ZERO.           
009140     05  W-IDARTNR-ASEQ-MIN      PIC S9(9)  COMP-3  VALUE ZERO.           
009141                                                                          
009142   03  WDD8ASEQ-MAX-X.                                                    
009143     05  W-IDDC-ASEQ-MAX       PIC X(2)          VALUE SPACE.             
009144     05  W-ADBUFFOMR-ASEQ-MAX  PIC S9(3)  COMP-3 VALUE ZERO.              
009145     05  W-ADBUFFGANG-ASEQ-MAX PIC S9(3)  COMP-3 VALUE ZERO.              
009146     05  W-ADBUFFPL-ASEQ-MAX   PIC S9(5)  COMP-3 VALUE ZERO.              
009147     05  W-DABUFPAF-ASEQ-MAX   PIC  9(8)  COMP-3 VALUE 99999999.          
009148     05  W-IDARTNR-ASEQ-MAX    PIC S9(9)  COMP-3 VALUE 999999999.         
009149                                                                          
009150     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009310     88  SEGMENT-FOUND                       VALUE '  '.                  
009320     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009330     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009340     88  END-OF-DATABASE                     VALUE 'GB'.                  
009900     SKIP2                                                                
010000 01  GOOD-STATUSCODES.                                                    
010100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(64).                               
010400 01  SSA2                        PIC X(64).                               
010500     EJECT                                                                
010600*    --- IMS FUNKTIONSKODER                                               
010700*01  -COPY W0003                                                          
010800     EJECT                                                                
011000*    ---  DLI INPUT-OUTPUT AREA                                           
011100                                                                          
011205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
011206 01  DLI-IO-WDD811.                                                       
011207*    03  -COPY WDD811                                                     
011208 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7A1'.                      
011209 01  DLI-IO-WDK7A1.                                                       
011210*    03  -COPY WDK7A1                                                     
011211 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ801'.                      
011212 01  DLI-IO-WDJ801.                                                       
011220*    03  -COPY WDJ801                                                     
011500     EJECT                                                                
011600 LINKAGE SECTION.                                                         
011700                                                                          
011800*01  -COPY W0009  -PRE MSG-                                               
011900*01  -COPY W0009  -PRE ALT-                                               
011901     EJECT                                                                
011902                                                                          
011903*01  -COPY W0008  -PRE WDD8-                                              
011904     05  FILLER                  PIC X.                                   
011905                                                                          
011906*01  -COPY W0008  -PRE WDK7-                                              
011907     05  FILLER                  PIC X.                                   
011908                                                                          
011909*01  -COPY W0008  -PRE WDJ8-                                              
011910     05  FILLER                  PIC X.                                   
012200     EJECT                                                                
012301 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDD8-PCB WDK7-PCB              
012302     WDJ8-PCB.                                                            
012303 MAIN SECTION.                                                            
012304     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDD8-PCB WDK7-PCB              
012310     WDJ8-PCB.                                                            
012400                                                                          
012700     PERFORM A-INIT                                                       
012900     PERFORM B-READ-WRITE-INFO                                            
014000     PERFORM Z-FINIT                                                      
014100                                                                          
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015200     SKIP2                                                                
015600                                                                          
015800                                                                          
015900     ACCEPT DAGENS-DATUM FROM DATE                                        
016000     ACCEPT PARM-SYSIN FROM SYSIN                                         
016010     DISPLAY 'PARM-SYSIN : ' PARM-SYSIN                                   
016100     UNSTRING PARM-SYSIN DELIMITED BY ','                                 
016200       INTO PARM-IDDC                                                     
016300            PARM-ADLAGOMR                                                 
016400            PARM-ADGANG-FOM                                               
016500            PARM-ADGANG-TOM                                               
016600            PARM-ADSEC-FOM                                                
016601            PARM-ADSEC-TOM                                                
016602            PARM-ADLEVEL-FOM                                              
016603            PARM-ADLEVEL-TOM                                              
016604            PARM-KDLOC                                                    
016605            PARM-KDFREQ                                                   
016606            PARM-TELOC                                                    
016607            PARM-KDPRT                                                    
016608     DISPLAY 'PARM-IDDC        : ' PARM-IDDC                              
016609     DISPLAY 'PARM-ADLAGOMR    : ' PARM-ADLAGOMR                          
016610     DISPLAY 'PARM-ADGANG-FOM  : ' PARM-ADGANG-FOM                        
016611     DISPLAY 'PARM-ADGANG-TOM  : ' PARM-ADGANG-TOM                        
016612     DISPLAY 'PARM-ADSEC-FOM   : ' PARM-ADSEC-FOM                         
016613     DISPLAY 'PARM-ADSEC-TOM   : ' PARM-ADSEC-TOM                         
016614     DISPLAY 'PARM-ADLEVEL-FOM : ' PARM-ADLEVEL-FOM                       
016615     DISPLAY 'PARM-ADLEVEL-TOM : ' PARM-ADLEVEL-TOM                       
016616     DISPLAY 'PARM-KDLOC       : ' PARM-KDLOC                             
016617     DISPLAY 'PARM-KDFREQ      : ' PARM-KDFREQ                            
016618     DISPLAY 'PARM-TELOC       : ' PARM-TELOC                             
016619     DISPLAY 'PARM-KDPRT       : ' PARM-KDPRT                             
016620*    -- ÖPPNINGSANROP TILL PRINT-SUBPROGRAM                               
016621     SKIP2                                                                
016622     MOVE '6L'        TO W-IDPRTLST(1:2)                                  
016623     MOVE PARM-KDPRT  TO W-IDPRTLST(3:3)                                  
016624     MOVE SPACE       TO W-IDPRTLST(6:3)                                  
016625                                                                          
016626     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
016627                         PRT-OPEN                                         
016628                         W-IDPRTLST                                       
016630                         ALT-PCB                                          
016640                         PRT-FILLER                                       
016650                         PRT-FILLER                                       
016700     .                                                                    
016800     EJECT                                                                
016810 B-READ-WRITE-INFO SECTION.                                               
016892*                                                                         
016893     MOVE PARM-IDDC             TO W-LOC-IDDC-MAX                         
016894     MOVE PARM-ADLAGOMR         TO W-LOC-ADLAGOMR-MAX                     
016895     MOVE PARM-ADGANG-TOM       TO W-LOC-ADGANG-MAX                       
016896     MOVE PARM-ADSEC-TOM        TO W-LOC-ADSEC-MAX                        
016897     MOVE PARM-ADLEVEL-TOM      TO W-LOC-ADLEVEL-MAX                      
016898     MOVE 9                     TO W-LOC-ADSEQ-MAX                        
016899     MOVE PARM-IDDC             TO W-LOC-IDDC-MIN                         
016900     MOVE PARM-ADLAGOMR         TO W-LOC-ADLAGOMR-MIN                     
016901     MOVE PARM-ADGANG-FOM       TO W-LOC-ADGANG-MIN                       
016902     MOVE PARM-ADSEC-FOM        TO W-LOC-ADSEC-MIN                        
016903     MOVE PARM-ADLEVEL-FOM      TO W-LOC-ADLEVEL-MIN                      
016904     MOVE ZERO                  TO W-LOC-ADSEQ-MIN                        
016905                                                                          
016906     MOVE 99999999              TO W-IDARTNR-MAX                          
016907                                                                          
016908     PERFORM  IMS-GU-WDJ8                                                 
016909                                                                          
016910     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE                     
016911                                                                          
016912      MOVE LOC-ADPLATS            TO WORK-ADPLATS                         
016913      MOVE LOC-TELOC              TO WORK-TELOC                           
016914      MOVE SPACES                 TO WORK-IDARTNR                         
016915      IF PARM-ADLEVEL-FOM <= WORK-ADLEVEL               AND               
016916         PARM-ADLEVEL-TOM >= WORK-ADLEVEL               AND               
016917         PARM-ADSEC-FOM <= WORK-ADSEC                   AND               
016918         PARM-ADSEC-TOM >= WORK-ADSEC                   AND               
016919         (PARM-KDLOC   = " " OR  PARM-KDLOC = LOC-KDLOC) AND              
016920         (PARM-KDFREQ = 0    OR PARM-KDFREQ = LOC-KDFREQ) AND             
016921         (PARM-TELOC   = " " OR PARM-TELOC = WORK-TELOC)                  
016922         MOVE LOC-IDDC                  TO W-IDDC-MIN                     
016923                                     W-IDDC-MAX                           
016924         MOVE LOC-ADLAGOMR              TO W-ADLAGOMR-MIN                 
016925                                     W-ADLAGOMR-MAX                       
016926         MOVE LOC-ADGANG                TO W-ADGANG-MIN                   
016927                                     W-ADGANG-MAX                         
016928         MOVE LOC-ADPLATS               TO W-ADPLATS-MIN                  
016929                                     W-ADPLATS-MAX                        
016930         PERFORM        IMS-GU-WDK7A1                                     
016931         IF LOC-KDLOC        =  PRIME OR MIXED                            
016932            PERFORM UNTIL SEGMENT-MISSING OR                              
016933                          END-OF-DATABASE                                 
016934               ADD 1 TO TAB-INDX                                          
016935                        TAB-MAX-INDX                                      
016936               MOVE SEQA-IDARTNR                                          
016937                              TO WORK-IDARTNR-TAB (TAB-INDX)              
016938               INSPECT WORK-IDARTNR-TAB (TAB-INDX)                        
016939                REPLACING LEADING ZEROES BY SPACE                         
016940               PERFORM IMS-GN-WDK7A1                                      
016941            END-PERFORM                                                   
016942         END-IF                                                           
016943                                                                          
016944         IF LOC-KDLOC     =  (BUFFER OR MIXED) AND                        
016945            TAB-MAX-INDX = ZERO                                           
016946                                                                          
016947            MOVE LOC-IDDC               TO W-IDDC-ASEQ                    
016948            MOVE LOC-ADLAGOMR           TO W-ADBUFFOMR-ASEQ               
016949            MOVE LOC-ADGANG             TO W-ADBUFFGANG-ASEQ              
016950            MOVE LOC-ADPLATS            TO W-ADBUFFPL-ASEQ                
016951                                                                          
016952            PERFORM     IMS-GU-WDD811                                     
016953                                                                          
016954            IF SEGMENT-FOUND                                              
016955               MOVE SEQA-IDARTNR        TO WORK-IDARTNR                   
016956               INSPECT WORK-IDARTNR REPLACING LEADING                     
016957                  ZEROES BY SPACE                                         
016958            END-IF                                                        
016959         END-IF                                                           
016960         IF TAB-MAX-INDX = ZERO                                           
016961           MOVE LOC-ADLAGOMR         TO LIST-ADLAGOMR-LINE                
016962           MOVE LOC-ADGANG           TO LIST-ADGANG-LINE                  
016963           MOVE LOC-ADPLATS          TO LIST-ADPLATS-LINE                 
016964           MOVE LOC-KDLOC            TO LIST-KDLOC-LINE                   
016965           MOVE LOC-KDFREQ           TO LIST-KDFREQ-LINE                  
016966           MOVE LOC-KDSTOR           TO LIST-KDSTOR-LINE                  
016967           MOVE LOC-TELOC            TO LIST-TELOC-LINE                   
016968           MOVE WORK-IDARTNR         TO LIST-IDARTNR-LINE                 
016969           PERFORM C-WRITE-LIST                                           
016970         ELSE                                                             
016971           MOVE LOC-ADLAGOMR         TO LIST-ADLAGOMR-LINE                
016972           MOVE LOC-ADGANG           TO LIST-ADGANG-LINE                  
016973           MOVE LOC-ADPLATS          TO LIST-ADPLATS-LINE                 
016974           MOVE LOC-KDLOC            TO LIST-KDLOC-LINE                   
016975           MOVE LOC-KDFREQ           TO LIST-KDFREQ-LINE                  
016976           MOVE LOC-KDSTOR           TO LIST-KDSTOR-LINE                  
016977           MOVE LOC-TELOC            TO LIST-TELOC-LINE                   
016978           MOVE 1     TO TAB-INDX                                         
016979           PERFORM UNTIL TAB-INDX > TAB-MAX-INDX                          
016980              MOVE WORK-IDARTNR-TAB (TAB-INDX)                            
016981                                  TO LIST-IDARTNR-LINE                    
016982              ADD 1 TO TAB-INDX                                           
016983              PERFORM C-WRITE-LIST                                        
016984*             BLANKAR FÖLJDRADER PÅ LISTAN                                
016985              MOVE SPACE             TO LIST-ADLAGOMR-LINE                
016986              MOVE SPACE             TO LIST-ADGANG-LINE                  
016987              MOVE SPACE             TO LIST-ADPLATS-LINE                 
016988              MOVE SPACE             TO LIST-KDLOC-LINE                   
016989              MOVE SPACE             TO LIST-KDFREQ-LINE                  
016990              MOVE SPACE             TO LIST-KDSTOR-LINE                  
016991              MOVE SPACE             TO LIST-TELOC-LINE                   
016992           END-PERFORM                                                    
016993           MOVE ZERO              TO TAB-INDX                             
016994                                     TAB-MAX-INDX                         
016995                                                                          
016996         END-IF                                                           
016997       END-IF                                                             
016998       PERFORM IMS-GN-WDJ8                                                
016999     END-PERFORM                                                          
017000     .                                                                    
017001     EJECT                                                                
017002*                                                                         
017003 C-WRITE-LIST SECTION.                                                    
017004     IF INDX = ZERO OR INDX > MAX-INDX                                    
017005       ADD 1 TO SID-INDX                                                  
017006       MOVE SID-INDX           TO LIST-PAGE                               
017007       PERFORM CA-INITIERA-LISTA                                          
017008       PERFORM CB-SKRIV-RUBRIKER                                          
017009     END-IF                                                               
017010     PERFORM CC-SKRIV-DETALJRAD                                           
017011     .                                                                    
017012     SKIP3                                                                
017013 CA-INITIERA-LISTA SECTION.                                               
017014     MOVE AAR                  TO LIST-AAR                                
017015     MOVE MAN                  TO LIST-MAN                                
017016     MOVE DAG                  TO LIST-DAG                                
017017     MOVE PARM-IDDC            TO LIST-DC                                 
017018     MOVE PARM-ADLAGOMR        TO LIST-AREA                               
017019     MOVE PARM-ADGANG-FOM      TO LIST-AF                                 
017020     MOVE PARM-ADGANG-TOM      TO LIST-AT                                 
017021     MOVE PARM-ADSEC-FOM       TO LIST-SF                                 
017022     MOVE PARM-ADSEC-TOM       TO LIST-ST                                 
017023     MOVE PARM-ADLEVEL-FOM     TO LIST-LF                                 
017024     MOVE PARM-ADLEVEL-TOM     TO LIST-LT                                 
017025     MOVE PARM-KDLOC           TO LIST-TYPE                               
017026     MOVE PARM-KDFREQ          TO LIST-FREQ                               
017027     MOVE PARM-TELOC           TO LIST-SPEC                               
017028     .                                                                    
017029     EJECT                                                                
017030*                                                                         
017031 CB-SKRIV-RUBRIKER SECTION.                                               
017032***** SKRIV RUBRIK                                                        
017033     MOVE RUBRIKRAD-1         TO RAD                                      
017034     MOVE PRT-NYSIDA-RAD4     TO PRT-RADSKIP                              
017035     PERFORM S01-SKRIV-RAD                                                
017036*                                                                         
017037***** SKRIV RUBRIKRAD 2                                                   
017038     MOVE RUBRIKRAD-3         TO RAD                                      
017039     MOVE PRT-AFTER-2         TO PRT-RADSKIP                              
017040     PERFORM S01-SKRIV-RAD                                                
017041***** SKRIV RUBRIKRAD 3                                                   
017042     MOVE RUBRIKRAD-5         TO RAD                                      
017043     MOVE PRT-AFTER-2         TO PRT-RADSKIP                              
017044     PERFORM S01-SKRIV-RAD                                                
017045     MOVE 5 TO INDX                                                       
017046     .                                                                    
017047     EJECT                                                                
017048*                                                                         
017049 CC-SKRIV-DETALJRAD SECTION.                                              
017050***** SKRIV DETALJRAD                                                     
017051     MOVE DETALJRAD           TO RAD                                      
017052     MOVE PRT-AFTER-1         TO PRT-RADSKIP                              
017053     PERFORM S01-SKRIV-RAD                                                
017054     ADD 1 TO INDX                                                        
017055     .                                                                    
017056     EJECT                                                                
017057*                                                                         
017058 S01-SKRIV-RAD SECTION.                                                   
017060     CALL W006PRS1 USING PRT-SPOOL-OVR                                    
017061                         PRT-WRITE                                        
017062                         W-IDPRTLST                                       
017063                         ALT-PCB                                          
017064                         PRT-RADSKIP                                      
017065                         RAD                                              
017067      .                                                                   
017068      EJECT                                                               
017080 Z-FINIT SECTION.                                                         
017100                                                                          
017401     SKIP2                                                                
017402     CALL W006PRS1 USING PRT-SPOOL-OVR  PRT-CLOSE  W-IDPRTLST             
017403                         ALT-PCB                                          
017410                         PRT-FILLER  PRT-FILLER                           
017500     .                                                                    
017600     EJECT                                                                
019700* --- IMS SEKTIONER ---                                                   
019710 IMS-GU-WDJ8 SECTION.                                                     
019720                                                                          
019730     STRING 'WDJ801  (WDJ801KY=>' W-WDJ8KY-MIN-X                          
019740                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
019750          DELIMITED BY SIZE INTO SSA1                                     
019760     MOVE '  GE' TO GOOD-STATUSCODES                                      
019770     CALL CBLTDLI USING GU WDJ8-PCB DLI-IO-WDJ801 SSA1                    
019780     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
019790     PERFORM IMS-STATUSCHECK                                              
019791     .                                                                    
019792     SKIP3                                                                
019793                                                                          
019794 IMS-GN-WDJ8 SECTION.                                                     
019795                                                                          
019796     STRING 'WDJ801  (WDJ801KY=>' W-WDJ8KY-MIN-X                          
019797                    '&WDJ801KY=<' W-WDJ8KY-MAX-X ')'                      
019798          DELIMITED BY SIZE INTO SSA1                                     
019799     MOVE '  GE' TO GOOD-STATUSCODES                                      
019800     CALL CBLTDLI USING GN WDJ8-PCB DLI-IO-WDJ801 SSA1                    
019801     MOVE WDJ8-STATUS-CODE TO STATUS-WS                                   
019820     PERFORM IMS-STATUSCHECK                                              
019830     .                                                                    
019840     SKIP3                                                                
019850                                                                          
019860 IMS-GU-WDK7A1 SECTION.                                                   
019870                                                                          
019880     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
019890                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
019891          DELIMITED BY SIZE INTO SSA1                                     
019892     MOVE '  GE' TO GOOD-STATUSCODES                                      
019893     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK7A1 SSA1                    
019894     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
019897     PERFORM IMS-STATUSCHECK                                              
019898     .                                                                    
019899     SKIP3                                                                
019900                                                                          
019901 IMS-GN-WDK7A1 SECTION.                                                   
019902                                                                          
019903     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
019904                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
019905          DELIMITED BY SIZE INTO SSA1                                     
019906     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
019907     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-WDK7A1 SSA1                    
019908     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
019911     PERFORM IMS-STATUSCHECK                                              
019912     .                                                                    
019913     SKIP3                                                                
019914                                                                          
019915 IMS-GU-WDD811 SECTION.                                                   
019916                                                                          
019917     STRING 'WDD811  (WDD8ASEQ =' W-WDD8ASEQ-X ')'                        
019918          DELIMITED BY SIZE INTO SSA1                                     
019919     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
019920     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD811 SSA1                    
019921     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
019924     PERFORM IMS-STATUSCHECK                                              
019925     .                                                                    
019930     EJECT                                                                
020100 IMS-STATUSCHECK SECTION.                                                 
020200     SKIP2                                                                
020300     SET STATUS-IX TO 1                                                   
020400     SEARCH GOOD-STATUS                                                   
020500       AT END                                                             
020600         STRING ' WRONG STATUSCODE FROM IMS:   ' STATUS-WS                
020700           DELIMITED BY SIZE INTO FELTEXT                                 
020800         DISPLAY FELTEXT                                                  
020900         CALL FELLOG                                                      
021000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
021100         CONTINUE                                                         
021200     END-SEARCH                                                           
021300     .                                                                    
