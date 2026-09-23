000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0116000.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   94/10/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER ARTIKELBASEN WDK6 MED SB.                                  
001000*        SKAPAR FILER MED SAMTLIGA DATAELEMENT FRÅN                       
001100*        OLIKA SEGMENT.                                                   
001200*                                                                         
001300*        W01160   SAMTLIGA DATAELEMENT FRÅN WDK601 OCH WDK611             
001600*        W01161   SAMTLIGA DATAELEMENT FRÅN WDK621                        
001700*        W01162   SAMTLIGA DATAELEMENT FRÅN WDK625                        
001800*        W01163   SAMTLIGA DATAELEMENT FRÅN WDK623                        
001810*        W01164X  SAMTLIGA DATAELEMENT FRÅN WDK613                        
001811*        W01166X  SAMTLIGA DATAELEMENT FRÅN WDK626                        
001812*        W01169X  SAMTLIGA DATAELEMENT FRÅN WDK629                        
001820*                 MEN MED DISPLAY FORMAT PÅ ALLA ELEMENT                  
001900*                                                                         
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- SAMTLIGA DATAELEMENT FRÅN WDK601 OCH WDK611                
002800     SELECT W01160                     ASSIGN TO W01160D1.                
003200     SKIP2                                                                
003300*          --- SAMTLIGA DATAELEMENT FRÅN WDK621                           
003400     SELECT W01161                     ASSIGN TO W01160D2.                
003500     SKIP2                                                                
003600*          --- SAMTLIGA DATAELEMENT FRÅN WDK625                           
003700     SELECT W01162                     ASSIGN TO W01160D3.                
003800     SKIP2                                                                
003900*          --- SAMTLIGA DATAELEMENT FRÅN WDK623                           
004000     SELECT W01163                     ASSIGN TO W01160D4.                
004100     EJECT                                                                
004140*          --- SAMTLIGA DATAELEMENT FRÅN WDK613                           
004150     SELECT W01164X                    ASSIGN TO W01160D6.                
004160     EJECT                                                                
004170*          --- SAMTLIGA DATAELEMENT FRÅN WDK626                           
004180     SELECT W01166X                    ASSIGN TO W01160D7.                
004190     EJECT                                                                
004191*          --- SAMTLIGA DATAELEMENT FRÅN WDK629                           
004192     SELECT W01169X                    ASSIGN TO W01160D8.                
004193     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W01160                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W01160 -PRE  W01160-  -L.                                 
005700     SKIP3                                                                
005800 FD  W01161                                                               
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200*01  POST -COPY W01161 -PRE  W01161-  -L.                                 
006300     SKIP3                                                                
006400 FD  W01162                                                               
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800*01  POST -COPY W01162 -PRE  W01162-  -L.                                 
006900     SKIP3                                                                
007000 FD  W01163                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  POST -COPY W01163 -PRE  W01163-  -L.                                 
007500     EJECT                                                                
007570 FD  W01164X                                                              
007580     RECORDING       F                                                    
007590     BLOCK CONTAINS  0.                                                   
007591                                                                          
007592*01  POST -COPY W01164X -PRE  W01164X- -L.                                
007593     EJECT                                                                
007594 FD  W01166X                                                              
007595     RECORDING       F                                                    
007596     BLOCK CONTAINS  0.                                                   
007597                                                                          
007598*01  POST -COPY W01166X -PRE  W01166X- -L.                                
007599     EJECT                                                                
007600 FD  W01169X                                                              
007610     RECORDING       F                                                    
007620     BLOCK CONTAINS  0.                                                   
007630                                                                          
007640*01  POST -COPY W01169X -PRE  W01169X- -L.                                
007650     EJECT                                                                
007700 WORKING-STORAGE SECTION.                                                 
008000 77  IDPGM                       PIC X(8)    VALUE 'W0116000'.            
008100 77  JA                          PIC X       VALUE 'J'.                   
008200 77  NEJ                         PIC X       VALUE 'N'.                   
008400                                                                          
008500 01  ARBETSAREOR.                                                         
008600     03  WS-SPAR-IDARTNR         PIC S9(9)   VALUE ZERO COMP-3.           
008700     03  IX                      PIC S9(3)   VALUE ZERO COMP-3.           
011300     EJECT                                                                
011400 01  DYNAMISKA-SUBPROGRAM.                                                
011500*                                                                         
011600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012000     SKIP2                                                                
012100*    --- PARAMETRAR TILL ABEND                                            
012200                                                                          
012300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012500     SKIP2                                                                
012600 01  FELTEXT.                                                             
012700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012900     EJECT                                                                
013000                                                                          
013100*    --- PARAMETRAR TILL POSTSUM                                          
013200                                                                          
013300*01  -COPY W0005   -PRE  POSTSUM-                                         
013400     EJECT                                                                
013500 01  W01160-AREA-START           PIC X(24)   VALUE                        
013600                                 'W01160-AREA-START  '.                   
013700     SKIP2                                                                
013800                                                                          
013900 01  W01160-AREA.                                                         
014000     03  CDC-UPPGIFTER.                                                   
014100         05 -COPY W01160                                                  
014200                                                                          
014700     EJECT                                                                
014800 01  W01161-AREA-START           PIC X(24)   VALUE                        
014900                                 'W01161-AREA-START  '.                   
015000     SKIP2                                                                
015100                                                                          
015200 01  W01161-AREA.                                                         
015300     03  PRIS-UPPGIFTER.                                                  
015400         05 -COPY W01161                                                  
015500                                                                          
015600     EJECT                                                                
015700 01  W01162-AREA-START           PIC X(24)   VALUE                        
015800                                 'W01162-AREA-START  '.                   
015900 01  W01162-AREA.                                                         
016000     03  NOT-UPPGIFTER.                                                   
016100         05 -COPY W01162                                                  
016200                                                                          
016300                                                                          
016400     EJECT                                                                
016500 01  W01163-AREA-START           PIC X(24)   VALUE                        
016600                                 'W01163-AREA-START  '.                   
016700 01  W01163-AREA.                                                         
016800     03  AVT-UPPGIFTER.                                                   
016900         05 -COPY W01163                                                  
017000                                                                          
017100                                                                          
017161 01  W01164X-AREA.                                                        
017162     03  EMB-UPPGIFTER-EXTRACT.                                           
017163         05 -COPY W01164X                                                 
045801                                                                          
045802 01  W01166X-AREA.                                                        
045803     03  JUST-UPPGIFTER-EXTRACT.                                          
045804         05 -COPY W01166X                                                 
045805                                                                          
045806 01  W01169X-AREA.                                                        
045807     03  CREF-UPPGIFTER-EXTRACT.                                          
045808         05 -COPY W01169X                                                 
045809                                                                          
045810     EJECT                                                                
045811*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
045812*                                                                         
045813     EJECT                                                                
045814 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
045815     SKIP3                                                                
045816 01  NYCKLAR-TILL-DLI.                                                    
045817     03  W-IDARTNR-X.                                                     
045818        05 W-IDARTNR            PIC S9(9)  COMP-3.                        
045819     EJECT                                                                
045820                                                                          
045821     SKIP2                                                                
045822*    --- STATUS-KOD FRÅN IMS                                              
045823 01  STATUS-WS                   PIC XX.                                  
045824     88  SEGMENT-FINNS                       VALUE '  '.                  
045825     88  SEGMENT-SLUT                        VALUE 'GB'.                  
045826     SKIP2                                                                
045827 01  GODK-STATUSKODER.                                                    
045828     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
045829     SKIP3                                                                
045830 01  SSA1                        PIC X(64).                               
045831 01  SSA2                        PIC X(64).                               
045832     EJECT                                                                
045833*    --- IMS FUNKTIONSKODER                                               
045834*01  -COPY W0003                                                          
045835     EJECT                                                                
045836*    ---  DLI INPUT-OUTPUT AREA                                           
045837 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
045838     SKIP3                                                                
045839 01  DLI-IO-AREA.                                                         
045840     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
045841                                                                          
045842     03  WDK601    REDEFINES  IO-AREA.                                    
045843         05  -COPY WDK601                                                 
045844                                                                          
045845     03  WDK611    REDEFINES  IO-AREA.                                    
045846         05  -COPY WDK611                                                 
045847                                                                          
045848     03  WDK613    REDEFINES  IO-AREA.                                    
045849         05  -COPY WDK613                                                 
045850                                                                          
045851     03  WDK621    REDEFINES  IO-AREA.                                    
045852         05  -COPY WDK621                                                 
045853                                                                          
045854     03  WDK623    REDEFINES  IO-AREA.                                    
045855         05  -COPY WDK623                                                 
045856                                                                          
045857     03  WDK625    REDEFINES  IO-AREA.                                    
045858         05  -COPY WDK625                                                 
045859                                                                          
045860     03  WDK626    REDEFINES  IO-AREA.                                    
045861         05  -COPY WDK626                                                 
045862                                                                          
045863     03  WDK629    REDEFINES  IO-AREA.                                    
045864         05  -COPY WDK629                                                 
045865                                                                          
045866     EJECT                                                                
045867 LINKAGE SECTION.                                                         
045868                                                                          
045869     EJECT                                                                
045870*01  -COPY W0008  -PRE WDK6-                                              
045871     05  FILLER                  PIC X.                                   
045872     EJECT                                                                
045873 PROCEDURE DIVISION  USING WDK6-PCB.                                      
045874     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
045875                                                                          
045876     PERFORM A-INIT                                                       
045877     PERFORM IMS-GET-WDK6                                                 
045878     PERFORM UNTIL SEGMENT-SLUT                                           
045880        EVALUATE WDK6-SEG-NAME-FB                                         
045881           WHEN 'WDK601  '                                                
045882              IF WS-SPAR-IDARTNR > ZERO                                   
045883                 PERFORM S11-SKRIV-W01160                                 
045885                 INITIALIZE W01160-AREA                                   
045887              END-IF                                                      
045888              MOVE ART-IDARTNR TO WS-SPAR-IDARTNR                         
045890              PERFORM B-FLYTTA-WDK601                                     
045891           WHEN 'WDK611  '                                                
045892              PERFORM C-FLYTTA-WDK611                                     
045893           WHEN 'WDK621  '                                                
045894              PERFORM E-FLYTTA-WDK621                                     
045895              PERFORM S12-SKRIV-W01161                                    
045896           WHEN 'WDK623  '                                                
045897              PERFORM G-FLYTTA-WDK623                                     
045898              PERFORM S14-SKRIV-W01163                                    
045899           WHEN 'WDK625  '                                                
045900              PERFORM F-FLYTTA-WDK625                                     
045901              PERFORM S13-SKRIV-W01162                                    
045902           WHEN 'WDK626  '                                                
045904              PERFORM H-FLYTTA-WDK626                                     
045905              PERFORM S17-SKRIV-W01166X                                   
045906           WHEN 'WDK629  '                                                
045908              PERFORM I-FLYTTA-WDK629                                     
045909              PERFORM S18-SKRIV-W01169X                                   
045910           WHEN 'WDK613  '                                                
045911              PERFORM D-FLYTTA-WDK613                                     
045912              PERFORM S16-SKRIV-W01164X                                   
045914         END-EVALUATE                                                     
045915         PERFORM IMS-GET-WDK6                                             
045916     END-PERFORM                                                          
045917     PERFORM S11-SKRIV-W01160                                             
045919     PERFORM Z-FINIT                                                      
045920     MOVE ZERO TO RETURN-CODE                                             
045921     GOBACK                                                               
045922     .                                                                    
045923     EJECT                                                                
045924                                                                          
045925 A-INIT SECTION.                                                          
045926                                                                          
045927     OPEN OUTPUT W01160                                                   
045929                 W01161                                                   
045930                 W01162                                                   
045931                 W01163                                                   
045932                 W01164X                                                  
045933                 W01166X                                                  
045934                 W01169X                                                  
045935                                                                          
045936     INITIALIZE W01160-AREA                                               
045938                                                                          
045939                                                                          
045940     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
045941     .                                                                    
045942     EJECT                                                                
045943                                                                          
045944  B-FLYTTA-WDK601 SECTION.                                                
045945                                                                          
045946     MOVE ART-WDK601           TO CLAG-WDK601 IN CLAG-W01160              
045947                                                                          
045973     .                                                                    
045974     EJECT                                                                
045975                                                                          
045976  C-FLYTTA-WDK611 SECTION.                                                
045977                                                                          
045978     MOVE CLAG-WDK611          IN DLI-IO-AREA     TO                      
045979          CLAG-WDK611          IN CLAG-W01160                             
046046     .                                                                    
046047     EJECT                                                                
046048                                                                          
046049  D-FLYTTA-WDK613 SECTION.                                                
046050                                                                          
046051     IF EMB-IDARTNR-EMB IN DLI-IO-AREA NOT NUMERIC                        
046052        MOVE +0                TO EMB-IDARTNR-EMB IN DLI-IO-AREA          
046053     END-IF                                                               
046054     IF EMB-KVQPACK-EMB IN DLI-IO-AREA NOT NUMERIC                        
046055        MOVE +0                TO EMB-KVQPACK-EMB IN DLI-IO-AREA          
046056     END-IF                                                               
046057     IF EMB-KDEMBKOD    IN DLI-IO-AREA NOT NUMERIC                        
046058        MOVE +0                TO EMB-KDEMBKOD    IN DLI-IO-AREA          
046059     END-IF                                                               
046060                                                                          
046061     MOVE WS-SPAR-IDARTNR      TO EMB-IDARTNR     IN EMB-W01164X          
046062                                                                          
046063     MOVE EMB-KDEMBKEY         IN DLI-IO-AREA                             
046064                               TO EMB-KDEMBKEY    IN EMB-W01164X          
046065     MOVE EMB-IDARTNR-EMB      IN DLI-IO-AREA                             
046066                               TO EMB-IDARTNR-EMB IN EMB-W01164X          
046067     MOVE EMB-KVQPACK-EMB      IN DLI-IO-AREA                             
046068                               TO EMB-KVQPACK-EMB IN EMB-W01164X          
046069     MOVE EMB-KDEMBKOD         IN DLI-IO-AREA                             
046070                               TO EMB-KDEMBKOD    IN EMB-W01164X          
046071     .                                                                    
046080     EJECT                                                                
046100                                                                          
047700  E-FLYTTA-WDK621 SECTION.                                                
047800                                                                          
047900     MOVE WS-SPAR-IDARTNR      TO PRL-IDARTNR                             
048000     MOVE CORR WDK621          TO PRL-W01161                              
048100     .                                                                    
048200     EJECT                                                                
048300                                                                          
048400                                                                          
048500  F-FLYTTA-WDK625 SECTION.                                                
048600                                                                          
048700     MOVE WS-SPAR-IDARTNR      TO NOT-IDARTNR                             
048800     MOVE CORR NOT-WDK625      TO NOT-W01162                              
048900     .                                                                    
049000     EJECT                                                                
049100                                                                          
049200  G-FLYTTA-WDK623 SECTION.                                                
049300                                                                          
049400     MOVE WS-SPAR-IDARTNR      TO AVT-IDARTNR                             
049500     MOVE CORR AVT-WDK623      TO AVT-W01163                              
049600     .                                                                    
049700     EJECT                                                                
049800                                                                          
049900                                                                          
049910 H-FLYTTA-WDK626 SECTION.                                                 
049920                                                                          
049930     MOVE WS-SPAR-IDARTNR      TO JUST-IDARTNR                            
049931                               IN JUST-W01166X                            
049943     MOVE JUST-REPBJUST        IN DLI-IO-AREA                             
049944                               TO JUST-REPBJUST   IN JUST-W01166X         
049945     MOVE JUST-TIPBJUST-CENTR  IN DLI-IO-AREA                             
049946                               TO JUST-TIPBJUST-CENTR                     
049947                                                  IN JUST-W01166X         
049948     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 2                          
049949        MOVE JUST-KVPB-JUST    IN DLI-IO-AREA(IX)                         
049950                               TO JUST-KVPB-JUST                          
049951                               IN JUST-W01166X(IX)                        
049952        MOVE JUST-TIPBJUST     IN DLI-IO-AREA(IX)                         
049953                               TO JUST-TIPBJUST                           
049954                               IN JUST-W01166X(IX)                        
049955     END-PERFORM                                                          
049956     MOVE JUST-DAMANSEA        IN DLI-IO-AREA                             
049957                               TO JUST-DAMANSEA   IN JUST-W01166X         
049958     MOVE JUST-DASPSEA         IN DLI-IO-AREA                             
049959                               TO JUST-DASPSEA    IN JUST-W01166X         
049960     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 12                         
049961        MOVE JUST-RESEASON     IN DLI-IO-AREA(IX)                         
049962                               TO JUST-RESEASON                           
049963                               IN JUST-W01166X(IX)                        
049964     END-PERFORM                                                          
049965     .                                                                    
049966     EJECT                                                                
049970                                                                          
049980 I-FLYTTA-WDK629 SECTION.                                                 
049990                                                                          
049991     MOVE WS-SPAR-IDARTNR      TO CREF-IDARTNR                            
049992                               IN CREF-W01169X                            
049997     MOVE CREF-IDDC-REF        IN DLI-IO-AREA                             
049998                               TO CREF-IDDC-REF   IN CREF-W01169X         
049999     MOVE CREF-FLFLYG          IN DLI-IO-AREA                             
050000                               TO CREF-FLFLYG     IN CREF-W01169X         
050001     MOVE CREF-FLPB-FLYTT      IN DLI-IO-AREA                             
050002                               TO CREF-FLPB-FLYTT IN CREF-W01169X         
050003     MOVE CREF-FLREFBEO        IN DLI-IO-AREA                             
050004                               TO CREF-FLREFBEO   IN CREF-W01169X         
050005     MOVE CREF-FLREFILL        IN DLI-IO-AREA                             
050006                               TO CREF-FLREFILL   IN CREF-W01169X         
050007     MOVE CREF-FLREFNYO        IN DLI-IO-AREA                             
050008                               TO CREF-FLREFNYO   IN CREF-W01169X         
050009     MOVE CREF-FLWILSON        IN DLI-IO-AREA                             
050010                               TO CREF-FLWILSON   IN CREF-W01169X         
050011     MOVE CREF-FLBUYUPD        IN DLI-IO-AREA                             
050012                               TO CREF-FLBUYUPD   IN CREF-W01169X         
050013     MOVE CREF-IDPERSON-BUY    IN DLI-IO-AREA                             
050014                               TO CREF-IDPERSON-BUY                       
050015                                                  IN CREF-W01169X         
050016     MOVE CREF-FLTABUPD        IN DLI-IO-AREA                             
050017                               TO CREF-FLTABUPD   IN CREF-W01169X         
050018     MOVE CREF-IDREFTAB        IN DLI-IO-AREA                             
050019                               TO CREF-IDREFTAB   IN CREF-W01169X         
050020     MOVE CREF-KVPB-PLAN       IN DLI-IO-AREA                             
050021                               TO CREF-KVPB-PLAN  IN CREF-W01169X         
050022     MOVE CREF-KDREFSTA        IN DLI-IO-AREA                             
050023                               TO CREF-KDREFSTA   IN CREF-W01169X         
050024     MOVE CREF-KVREFOVL        IN DLI-IO-AREA                             
050025                               TO CREF-KVREFOVL   IN CREF-W01169X         
050026     MOVE CREF-KVREFPKT        IN DLI-IO-AREA                             
050027                               TO CREF-KVREFPKT   IN CREF-W01169X         
050028     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 12                         
050029        MOVE CREF-RESEASON-PLAN                                           
050030                               IN DLI-IO-AREA(IX)                         
050031                               TO CREF-RESEASON-PLAN                      
050032                               IN CREF-W01169X(IX)                        
050033     END-PERFORM                                                          
050034     MOVE CREF-TIORDREG        IN DLI-IO-AREA                             
050035                               TO CREF-TIORDREG   IN CREF-W01169X         
050036     MOVE CREF-TIREFEFT        IN DLI-IO-AREA                             
050037                               TO CREF-TIREFEFT   IN CREF-W01169X         
050038     MOVE CREF-TIREFPAF        IN DLI-IO-AREA                             
050039                               TO CREF-TIREFPAF   IN CREF-W01169X         
050040     MOVE CREF-TIREFPKT        IN DLI-IO-AREA                             
050041                               TO CREF-TIREFPKT   IN CREF-W01169X         
050042     MOVE CREF-TIREFSTA        IN DLI-IO-AREA                             
050043                               TO CREF-TIREFSTA   IN CREF-W01169X         
050044     MOVE CREF-TIREFSTO        IN DLI-IO-AREA                             
050045                               TO CREF-TIREFSTO   IN CREF-W01169X         
050046                                                                          
050047     .                                                                    
050048     EJECT                                                                
050049                                                                          
050050                                                                          
050060 Z-FINIT SECTION.                                                         
050100     CLOSE W01160                                                         
050300           W01161                                                         
050400           W01162                                                         
050500           W01163                                                         
050520           W01164X                                                        
050530           W01166X                                                        
050540           W01169X                                                        
050600     SKIP2                                                                
050700     MOVE 'S' TO POSTSUM-OPKOD                                            
050800     CALL POSTSUM USING POSTSUM-PARM                                      
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200                                                                          
051300 S11-SKRIV-W01160 SECTION.                                                
051400                                                                          
052000     WRITE W01160-POST FROM W01160-AREA                                   
052100                                                                          
052200     MOVE 'W01160' TO POSTSUM-FDNAMN                                      
052300     MOVE 'W01160D1' TO POSTSUM-DDNAMN2                                   
052400     CALL POSTSUM USING POSTSUM-PARM                                      
052500     .                                                                    
052600     EJECT                                                                
052700                                                                          
053900 S12-SKRIV-W01161 SECTION.                                                
054000                                                                          
054100     WRITE W01161-POST FROM W01161-AREA                                   
054200                                                                          
054300     MOVE 'W01161' TO POSTSUM-FDNAMN                                      
054400     MOVE 'W01160D2' TO POSTSUM-DDNAMN2                                   
054500     CALL POSTSUM USING POSTSUM-PARM                                      
054600     .                                                                    
054700     EJECT                                                                
054800                                                                          
054900                                                                          
055000 S13-SKRIV-W01162 SECTION.                                                
055100                                                                          
055200     WRITE W01162-POST FROM W01162-AREA                                   
055300                                                                          
055400     MOVE 'W01162' TO POSTSUM-FDNAMN                                      
055500     MOVE 'W01160D3' TO POSTSUM-DDNAMN2                                   
055600     CALL POSTSUM USING POSTSUM-PARM                                      
055700     .                                                                    
055800                                                                          
055900 S14-SKRIV-W01163 SECTION.                                                
056000                                                                          
056100     WRITE W01163-POST FROM W01163-AREA                                   
056200                                                                          
056300     MOVE 'W01163' TO POSTSUM-FDNAMN                                      
056400     MOVE 'W01160D4' TO POSTSUM-DDNAMN2                                   
056500     CALL POSTSUM USING POSTSUM-PARM                                      
056600     .                                                                    
056601                                                                          
056691 S16-SKRIV-W01164X SECTION.                                               
056692                                                                          
056693     WRITE W01164X-POST FROM W01164X-AREA                                 
056694                                                                          
056695     MOVE 'W01164X'  TO POSTSUM-FDNAMN                                    
056696     MOVE 'W01160D6' TO POSTSUM-DDNAMN2                                   
056697     CALL POSTSUM USING POSTSUM-PARM                                      
056698     .                                                                    
056699                                                                          
056700 S17-SKRIV-W01166X SECTION.                                               
056701                                                                          
056703     WRITE W01166X-POST FROM W01166X-AREA                                 
056704                                                                          
056705     MOVE 'W01166X'  TO POSTSUM-FDNAMN                                    
056706     MOVE 'W01160D7' TO POSTSUM-DDNAMN2                                   
056707     CALL POSTSUM USING POSTSUM-PARM                                      
056708     .                                                                    
056709                                                                          
056710 S18-SKRIV-W01169X SECTION.                                               
056711                                                                          
056713     WRITE W01169X-POST FROM W01169X-AREA                                 
056714                                                                          
056715     MOVE 'W01169X'  TO POSTSUM-FDNAMN                                    
056716     MOVE 'W01160D8' TO POSTSUM-DDNAMN2                                   
056717     CALL POSTSUM USING POSTSUM-PARM                                      
056718     .                                                                    
056719                                                                          
056720     EJECT                                                                
056800                                                                          
056900                                                                          
057000* --- IMS SEKTIONER ---                                                   
057100     SKIP3                                                                
057200     EJECT                                                                
057300                                                                          
057400                                                                          
057500 IMS-GET-WDK6   SECTION.                                                  
057600                                                                          
057700     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
057800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
057900     MOVE '  GAGKGBGE' TO GODK-STATUSKODER                                
058000     PERFORM IMS-STATUSKONTROLL                                           
058100     .                                                                    
058200     EJECT                                                                
058300 IMS-STATUSKONTROLL SECTION.                                              
058400                                                                          
058500     SET STATUS-IX TO 1                                                   
058600     SEARCH GODK-STATUS                                                   
058700       AT END                                                             
058800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
058900         DISPLAY FELTEXT                                                  
059000         CALL FELLOG                                                      
059100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
059200         CONTINUE                                                         
059300     END-SEARCH                                                           
059400     .                                                                    
