000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3018300.                                                
000400 AUTHOR.         INGVAR SKJELBRED.                                        
000500 DATE-WRITTEN.   96/09/26.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKAPA PROFORMAFAKTUROR FÖR ATT TRANSPORTERA OBJEKT               
001000*        MELLAN NDC:ER OCH SDC (SITTARD) ELLER RENOVÖRER I ENGLAND        
001100*                                                                         
001200*        PROGRAMMET LÄSER      TABELL BYART                               
001300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001400*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001500*        PROGRAMMET LÄSER      WLXXLD                                     
001600*        PROGRAMMET LÄSER      WL3165 (HÄNDELSEDATABASEN)                 
001700*        PROGRAMMET LOGGAR SALDO FÖRÄNDRINGAR PÅ                          
001800*                              WLLOGA (WDL9)                              
001900*                                                                         
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W3T183                                              
002300*        MID:         W3I18301                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W3O18301                                            
002700*                                                                         
002800*    CHANGE LOG:                                                          
002900*      14/04/24 - REDDY RAHUL     - MOVED THE BUSINESS LOGIC TO           
003000*                                   NEW PROGRAM W3018310.                 
003100*                                   ETRACKER 10228590 - TORONTO.          
003200*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900*    -- CHECKED BY WY2000                                                 
004000     SKIP3                                                                
004100 77  IDPGM                       PIC X(08)   VALUE 'W3018300'.            
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005000 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77  MAX-INDX                    PIC S9(4)   VALUE +10  COMP SYNC.        
005200 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005300 77  MAX-KVRADER                 PIC S9(4)   VALUE +10  COMP SYNC.        
005400                                                                          
005500 01  ALL-SPACE.                                                           
005600     03  FILLER                  PIC X(80)  VALUE SPACE.                  
005700 01  ALL-PLUS.                                                            
005800     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
005900                                                                          
006000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006100     88  EGEN-MID                            VALUE '3183'.                
006200     88  GODK-MID                            VALUE '3181' '3182'          
006300                                                   '3183' '3184'          
006400                                                   '3185' '3186'          
006500                                                   '3187' '3188'          
006600                                                   '3189'.                
006700     88  HELP-MID                            VALUE '0551'.                
006800     EJECT                                                                
006900                                                                          
007000 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
007100 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
007200 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
007300 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
007400                                                                          
007500*                                                                         
007600 01  WS-IDARTNR                  PIC  X(9)   VALUE SPACE.                 
007700 01  WS-IDARTNR-NUM              PIC  9(9)   VALUE ZERO.                  
007800*                                                                         
007900     EJECT                                                                
008000                                                                          
008100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008200 01  FILLER                   PIC X(16) VALUE 'GENERELLA-SUBPGM'.         
008300 01  GENERELLA-SUBPROGRAM.                                                
008400     03  W3018310                PIC X(8)    VALUE 'W3018310'.            
008500     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
008600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL SUBPROGRAM WL01MCNV                              
009100 01  FILLER                      PIC X(16)   VALUE 'WL01MCNV '.           
009200*01 -COPY WL01MCNV                                                        
009300*    --- REQU AREA                                                        
009400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009500 01  REQU-AREA.                                                           
009600*    03 -COPY WZ01REQU                                                    
009700*    03 -COPY W30183I1                                                    
009800*    --- RESP AREA                                                        
009900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010000 01  RESP-AREA.                                                           
010100*    03 -COPY WZ01RESP                                                    
010200*    03 -COPY W30183O1                                                    
010300     EJECT                                                                
010400     SKIP3                                                                
010500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010800     SKIP3                                                                
010900*01 -COPY WMSGINIT                                                        
011000     SKIP3                                                                
011100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011400     SKIP3                                                                
011500*01  MID -COPY W3I18301                                                   
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011800     SKIP3                                                                
011900*01  -COPY WMSGAREA                                                       
012000     EJECT                                                                
012100     03  MOD REDEFINES MSG-AREA.                                          
012200*      05  -COPY W3O18301                                                 
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012500     SKIP3                                                                
012600*01  -COPY WMFSAREA                                                       
012700     EJECT                                                                
012800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012900*                                                                         
013000     EJECT                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013200     SKIP3                                                                
013300 01  SPAR-AREA.                                                           
013400     03  SPAR-IDTRANS            PIC X(4)    VALUE '3183'.                
013502     03  SPAR-NYCKLAR-ENTER      PIC X(13).                               
013602     03  SPAR-NYCKLAR-NEXT       PIC X(13).                               
013700     SKIP3                                                                
013800 01  NYCKLAR-TILL-BLAEDDRING.                                             
013900     03  W-NYCKEL-KEY-X.                                                  
014000         05  W-IDBYTKOL-KEY      PIC 9(3)    VALUE ZERO.                  
014100         05  W-IDARTNR-OBJ-KEY   PIC S9(9)   VALUE ZERO COMP-3.           
014202         05  W-IDDISTR-KEY       PIC 9(5)    VALUE ZERO.                  
014301     SKIP3                                                                
014401*    --- STATUS-KOD FRÅN IMS                                              
014501 01  STATUS-WS                   PIC XX.                                  
014601     88  SEGMENT-FINNS                       VALUE '  '.                  
014701     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014801     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014901     88  SEGMENT-SLUT                        VALUE 'GB'.                  
015001     SKIP2                                                                
015101 01  GODK-STATUSKODER.                                                    
015201     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015301     SKIP3                                                                
015401 01  SSA1                        PIC X(128).                              
015501     EJECT                                                                
015601*    --- IMS FUNKTIONSKODER                                               
015701*01  -COPY W0003                                                          
015801     EJECT                                                                
015901 LINKAGE SECTION.                                                         
016001*01  -COPY W0009   -PRE MSG-                                              
016101*01  -COPY W0009   -PRE ALT-                                              
016201     EJECT                                                                
016301 01  USEA-PCB                PIC X.                                       
016401 01  BENA-PCB                PIC X.                                       
016501 01  XXLD-PCB                PIC X.                                       
016601 01  ARTS-PCB                PIC X.                                       
016701 01  3165-PCB                PIC X.                                       
016801 01  LOGA-PCB                PIC X.                                       
016901 01  ARTC-PCB                PIC X.                                       
016901 01  WDB6-PCB                PIC X.                                       
017001     EJECT                                                                
017101 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
017201     BENA-PCB XXLD-PCB ARTS-PCB 3165-PCB LOGA-PCB                         
017301     ARTC-PCB WDB6-PCB.                                                   
017401 MAIN SECTION.                                                            
017501     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
017601     BENA-PCB XXLD-PCB ARTS-PCB 3165-PCB LOGA-PCB                         
017701     ARTC-PCB WDB6-PCB.                                                   
017801                                                                          
017901     PERFORM IMS-GET-MSG                                                  
018001     IF SEGMENT-FINNS                                                     
018101       PERFORM A-INIT                                                     
018201       PERFORM B-INIT-KEYS                                                
018301       PERFORM C-INIT-REQU                                                
018401       IF MFS-UPDATE                                                      
018501         SET REQU-UPDATE         TO TRUE                                  
018601         PERFORM E-SAMMA-SIDA                                             
018701       ELSE                                                               
018801         IF MFS-FIRST                                                     
018901           SET REQU-FIRST        TO TRUE                                  
019001           PERFORM MFS-RENSA-FAELT-IN                                     
019101         ELSE                                                             
019201           IF MFS-NEXT                                                    
019301             SET REQU-NEXT       TO TRUE                                  
019401             PERFORM D-NAESTA-SIDA                                        
019501           ELSE                                                           
019601             SET REQU-QUERY      TO TRUE                                  
019701             PERFORM E-SAMMA-SIDA                                         
019801           END-IF                                                         
019901         END-IF                                                           
020001       END-IF                                                             
020101       PERFORM F-CALL-BIZ-LOGIC-W3018310                                  
020201       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O18301 + 4                      
020301       PERFORM IMS-INSERT-MSG                                             
020401     END-IF                                                               
020501                                                                          
020601     MOVE ZERO TO RETURN-CODE                                             
020701     GOBACK                                                               
020801     .                                                                    
020901     EJECT                                                                
021001 A-INIT SECTION.                                                          
021101     MOVE 'A-INIT'               TO WS-SEKTION                            
021201                                                                          
021301     IF MSG-DUBBLA-TRANSKODER                                             
021401       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
021501                                 TO MID-W3I18301                          
021601       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
021701       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
021801     ELSE                                                                 
021901       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
022001                                 TO MID-W3I18301                          
022101       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
022201       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
022301     END-IF                                                               
022401                                                                          
022501     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
022601     MOVE MSG-IDPFK              TO MFS-IDPFK                             
022701     MOVE MFS-IDTRANS            TO W-IDTRANS                             
022801                                                                          
022901     MOVE LOW-VALUE              TO MSG-AREA                              
023001     MOVE 'W3O183N1'             TO MFS-IDMOD                             
023101     MOVE '3183'                 TO MOD-IDTRANS                           
023201     MOVE MFS-RENSA-FAELT        TO MOD-TEMFSFEL MOD-TEMFSINF             
023301                                                                          
023401     MOVE SPACE                  TO SPAR-NYCKLAR-ENTER                    
023501     MOVE SPACE                  TO SPAR-NYCKLAR-NEXT                     
023601                                                                          
023701     IF EGEN-MID OR HELP-MID                                              
023801       CONTINUE                                                           
023901     ELSE                                                                 
024001       MOVE SPACE                TO MFS-KDTRTYP                           
024101       MOVE '7'                  TO MFS-IDPFK                             
024201     END-IF                                                               
024301                                                                          
024401     MOVE ALL '+'                TO MSGI-WMSGINIT                         
024501     MOVE '001'                  TO MSGI-KDCALL                           
024601     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
024701     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
024801     MOVE '3183'                 TO MSGI-IDTRANS                          
024901                                                                          
025001     PERFORM MFS-FORM-ATTR                                                
025101                                                                          
025201     .                                                                    
025301     EJECT                                                                
025401 B-INIT-KEYS SECTION.                                                     
025501     MOVE 'B-INIT-KEYS'          TO WS-SEKTION                            
025601                                                                          
025701     IF MID-IDARTNR-OBJ-IN NOT = ALL '+' OR                               
025801        MID-IDBYTKOL-IN    NOT = ALL '+' OR                               
025901        MID-IDDISTR-IN     NOT = ALL '+'                                  
026001       MOVE SPACE                TO MFS-KDTRTYP                           
026101       MOVE '7'                  TO MFS-IDPFK                             
026201     END-IF                                                               
026301                                                                          
026401     IF EGEN-MID                                                          
026501       CONTINUE                                                           
026601     ELSE                                                                 
026701       MOVE ZERO                 TO MID-IDBYTKOL-IN                       
026801                                    MID-IDARTNR-OBJ-IN                    
026901                                    MID-IDDISTR-IN                        
027001     END-IF                                                               
027101                                                                          
027201     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027301     MOVE MSGI-IDDC              TO REQU-IDDC                             
027401                                                                          
027501     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
027601                                    REQU-IDSPRAK                          
027701                                                                          
027801     MOVE '101'                  TO REQU-IDMSGVER                         
027901     MOVE MSGI-IDUSER            TO REQU-IDUSER                           
028001                                                                          
028101     MOVE MSGI-SPAR-AREA         TO SPAR-AREA                             
028201                                                                          
028301     MOVE MFS-RENSA-FAELT        TO MOD-IDBYTKOL-IN                       
028401                                                                          
028501     IF MID-IDBYTKOL-IN = ALL '+'                                         
028601       INSPECT MID-IDBYTKOL-UT                                            
028701         REPLACING LEADING SPACE BY ZERO                                  
028801       MOVE MID-IDBYTKOL-UT      TO REQU-IDBYTKOL-KEY                     
028901     ELSE                                                                 
029001       INSPECT MID-IDBYTKOL-IN                                            
029101         REPLACING LEADING SPACE BY ZERO                                  
029201       MOVE MID-IDBYTKOL-IN      TO REQU-IDBYTKOL-KEY                     
029301     END-IF                                                               
029401                                                                          
029501     MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-OBJ-IN                    
029601                                                                          
029701     IF MID-IDARTNR-OBJ-IN = ALL '+'                                      
029801       MOVE MID-IDARTNR-OBJ-UT   TO WS-IDARTNR                            
029901       INSPECT WS-IDARTNR                                                 
030001          REPLACING LEADING SPACE BY ZERO                                 
030101     ELSE                                                                 
030201       MOVE MID-IDARTNR-OBJ-IN   TO WS-IDARTNR                            
030301       INSPECT WS-IDARTNR                                                 
030401          REPLACING LEADING SPACE BY ZERO                                 
030501     END-IF                                                               
030601                                                                          
030701     MOVE WS-IDARTNR             TO REQU-IDARTNR-OBJ-KEY                  
030801                                                                          
030901     MOVE MFS-RENSA-FAELT        TO MOD-IDDISTR-IN                        
031001                                                                          
031101     IF MID-IDDISTR-IN = ALL '+'                                          
031201       INSPECT MID-IDDISTR-UT                                             
031301         REPLACING LEADING SPACE BY ZERO                                  
031401       MOVE MID-IDDISTR-UT       TO REQU-IDDISTR-KEY                      
031501     ELSE                                                                 
031601       INSPECT MID-IDDISTR-IN                                             
031701         REPLACING LEADING SPACE BY ZERO                                  
031801       MOVE MID-IDDISTR-IN       TO REQU-IDDISTR-KEY                      
031901     END-IF                                                               
032001                                                                          
032101     IF EGEN-MID                                                          
032201       MOVE REQU-IDBYTKOL-KEY    TO MOD-IDBYTKOL-UT                       
032301       INSPECT MOD-IDBYTKOL-UT REPLACING LEADING ZERO BY SPACE            
032401       MOVE WS-IDARTNR           TO MOD-IDARTNR-OBJ-UT                    
032501       INSPECT MOD-IDARTNR-OBJ-UT REPLACING                               
032601          LEADING ZERO BY SPACE                                           
032701       MOVE REQU-IDDISTR-KEY     TO MOD-IDDISTR-UT                        
032801       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
032901     END-IF                                                               
033001                                                                          
033101     .                                                                    
033201     EJECT                                                                
033301 C-INIT-REQU SECTION.                                                     
033401     MOVE 'C-INIT-REQU'          TO WS-SEKTION                            
033501                                                                          
033601     MOVE MAX-KVRADER            TO REQU-KVRADER                          
033701     IF MID-IDARTNR-OBJ-UPPD = ALL '+'                                    
033801       MOVE ALL-PLUS             TO REQU-IDARTNR-OBJ-UPPD                 
033901     ELSE                                                                 
034001       MOVE MID-IDARTNR-OBJ-UPPD TO REQU-IDARTNR-OBJ-UPPD                 
034101     END-IF                                                               
034201     IF MID-KVRETUR-UPPD = ALL '+'                                        
034301       MOVE ALL-PLUS             TO REQU-KVRETUR-UPPD                     
034401     ELSE                                                                 
034501       MOVE MID-KVRETUR-UPPD     TO REQU-KVRETUR-UPPD                     
034601     END-IF                                                               
034701     IF MID-FLBYTKNR-UPPD = ALL '+'                                       
034801       MOVE ALL-PLUS             TO REQU-FLBYTKNR-UPPD                    
034901     ELSE                                                                 
035001       MOVE MID-FLBYTKNR-UPPD    TO REQU-FLBYTKNR-UPPD                    
035101     END-IF                                                               
035201     IF MID-IDARTNR-OBJ-BORT = ALL '+'                                    
035301       MOVE ALL-PLUS             TO REQU-IDARTNR-OBJ-BORT                 
035401     ELSE                                                                 
035501       MOVE MID-IDARTNR-OBJ-BORT TO REQU-IDARTNR-OBJ-BORT                 
035601     END-IF                                                               
035701     IF MID-KVRETUR-BORT = ALL '+'                                        
035801       MOVE ALL-PLUS             TO REQU-KVRETUR-BORT                     
035901     ELSE                                                                 
036001       MOVE MID-KVRETUR-BORT     TO REQU-KVRETUR-BORT                     
036101     END-IF                                                               
036201     IF MID-IDBYTKOL-BORT = ALL '+'                                       
036301       MOVE ALL-PLUS             TO REQU-IDBYTKOL-BORT                    
036401     ELSE                                                                 
036501       MOVE MID-IDBYTKOL-BORT    TO REQU-IDBYTKOL-BORT                    
036601     END-IF                                                               
036701     IF MID-VKORDBTO-FAKT-IN = ALL '+'                                    
036801       MOVE ALL-PLUS             TO REQU-VKORDBTO-FAKT-UPD                
036901     ELSE                                                                 
037001       MOVE MID-VKORDBTO-FAKT-IN TO REQU-VKORDBTO-FAKT-UPD                
037101     END-IF                                                               
037201     IF MID-VLORDBTO-FAKT-IN = ALL '+'                                    
037301       MOVE ALL-PLUS             TO REQU-VLORDBTO-FAKT-UPD                
037401     ELSE                                                                 
037501       MOVE MID-VLORDBTO-FAKT-IN TO REQU-VLORDBTO-FAKT-UPD                
037601     END-IF                                                               
037701     IF MID-GODK-FAKT = ALL '+'                                           
037801       MOVE ALL-PLUS             TO REQU-GODK-FAKT                        
037901     ELSE                                                                 
038001       MOVE MID-GODK-FAKT        TO REQU-GODK-FAKT                        
038101     END-IF                                                               
038201     .                                                                    
038301     EJECT                                                                
038401 D-NAESTA-SIDA SECTION.                                                   
038501     MOVE 'D-NAESTA-SIDA '       TO WS-SEKTION                            
038601                                                                          
038701     IF MSGI-IDTRANS = '3183'                                             
038801       IF SPAR-IDTRANS = '3183'                                           
038901         MOVE SPAR-NYCKLAR-NEXT  TO W-NYCKEL-KEY-X                        
039001         IF SPAR-NYCKLAR-NEXT = ALL ZERO                                  
039101           MOVE SPAR-NYCKLAR-ENTER                                        
039201                                 TO W-NYCKEL-KEY-X                        
039301         END-IF                                                           
039401         MOVE W-IDARTNR-OBJ-KEY  TO REQU-IDARTNR-OBJ-START                
039501         MOVE W-IDBYTKOL-KEY     TO REQU-IDBYTKOL-START                   
039601         MOVE W-IDDISTR-KEY      TO REQU-IDDISTR-START                    
039701       ELSE                                                               
039801         MOVE ZERO               TO REQU-IDARTNR-OBJ-START                
039901         MOVE ZERO               TO REQU-IDBYTKOL-START                   
040001         MOVE ZERO               TO REQU-IDDISTR-START                    
040101       END-IF                                                             
040201     ELSE                                                                 
040301       MOVE ZERO                 TO REQU-IDARTNR-OBJ-START                
040401       MOVE ZERO                 TO REQU-IDBYTKOL-START                   
040501       MOVE ZERO                 TO REQU-IDDISTR-START                    
040601     END-IF                                                               
040701     .                                                                    
040801     EJECT                                                                
040901 E-SAMMA-SIDA SECTION.                                                    
041001     MOVE 'E-SAMMA-SIDA '        TO WS-SEKTION                            
041101                                                                          
041201     IF EGEN-MID OR HELP-MID                                              
041301       IF MSGI-IDTRANS = '3183'                                           
041401         IF SPAR-IDTRANS = '3183'                                         
041501           MOVE SPAR-NYCKLAR-ENTER                                        
041601                                 TO W-NYCKEL-KEY-X                        
041701           MOVE W-IDARTNR-OBJ-KEY                                         
041801                                 TO REQU-IDARTNR-OBJ-START                
041901           MOVE W-IDBYTKOL-KEY   TO REQU-IDBYTKOL-START                   
042002           MOVE W-IDDISTR-KEY    TO REQU-IDDISTR-START                    
042102         ELSE                                                             
042202           MOVE ZERO             TO REQU-IDARTNR-OBJ-START                
042302                                    REQU-IDBYTKOL-START                   
042402                                    REQU-IDDISTR-START                    
042502         END-IF                                                           
042602       ELSE                                                               
042702         MOVE ZERO               TO REQU-IDARTNR-OBJ-START                
042802                                    REQU-IDBYTKOL-START                   
042902                                    REQU-IDDISTR-START                    
043002       END-IF                                                             
043102     ELSE                                                                 
043202       PERFORM MFS-RENSA-FAELT-IN                                         
043302     END-IF                                                               
043402     .                                                                    
043502     EJECT                                                                
043602 F-CALL-BIZ-LOGIC-W3018310 SECTION.                                       
043702                                                                          
043802     MOVE 'F-CALL-BIZ-LOGIC-W3018310'                                     
043902                                 TO WS-SEKTION                            
044002                                                                          
044102     CALL W3018310 USING REQU-AREA RESP-AREA MAX-KVRADER ALT-PCB          
044202                         BENA-PCB  XXLD-PCB  ARTS-PCB                     
044302                         3165-PCB  LOGA-PCB  ARTC-PCB WDB6-PCB            
044402                                                                          
044502     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
044602        RESP-IDMSG-INFO  NOT = SPACE                                      
044702       PERFORM FA-SET-MSG-AND-HILIGHT                                     
044802     END-IF                                                               
044902     PERFORM FB-MOVE-RESP-TO-MOD                                          
045002                                                                          
045102* SAVE START AND NEXT KEYS IN PROFILE DB                                  
045202     MOVE RESP-IDBYTKOL-START    TO W-IDBYTKOL-KEY                        
045302     MOVE RESP-IDARTNR-OBJ-START TO WS-IDARTNR-NUM                        
045402     MOVE WS-IDARTNR-NUM         TO W-IDARTNR-OBJ-KEY                     
045502     MOVE RESP-IDDISTR-START     TO W-IDDISTR-KEY                         
045602     MOVE W-NYCKEL-KEY-X         TO SPAR-NYCKLAR-ENTER                    
045702                                                                          
045802     MOVE RESP-IDBYTKOL-NEXT     TO W-IDBYTKOL-KEY                        
045902     MOVE RESP-IDARTNR-OBJ-NEXT  TO WS-IDARTNR-NUM                        
046002     MOVE WS-IDARTNR-NUM         TO W-IDARTNR-OBJ-KEY                     
046102     MOVE RESP-IDDISTR-NEXT      TO W-IDDISTR-KEY                         
046202     MOVE W-NYCKEL-KEY-X         TO SPAR-NYCKLAR-NEXT                     
046302                                                                          
046402     MOVE '002'                  TO MSGI-KDCALL                           
046502     MOVE '3183'                 TO MSGI-IDTRANS                          
046602     MOVE '3183'                 TO SPAR-IDTRANS                          
046702     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
046802     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
046902     MOVE SPAR-AREA              TO MSGI-SPAR-AREA                        
047002     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047102                                                                          
047202     .                                                                    
047302     EJECT                                                                
047402 FA-SET-MSG-AND-HILIGHT SECTION.                                          
047502                                                                          
047602     MOVE 'FA-SET-MSG-AND-HILIGHT'                                        
047702                                 TO WS-SEKTION                            
047802                                                                          
047902     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
048002     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
048102     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
048202                                                                          
048302     CALL WL01MCNV USING MCNV-AREA                                        
048402     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
048502     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
048602     .                                                                    
048702     EJECT                                                                
048802 FB-MOVE-RESP-TO-MOD SECTION.                                             
048902                                                                          
049002     MOVE 'FB-MOVE-RESP-TO-MOD'  TO WS-SEKTION                            
049102                                                                          
049202     MOVE RESP-IDARTNR-OBJ-UPPD-ATTR                                      
049302                                 TO MOD-IDARTNR-OBJ-UPPD-ATTR             
049402     IF RESP-IDARTNR-OBJ-UPPD = SPACE                                     
049502       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-OBJ-UPPD                  
049602     ELSE                                                                 
049702       IF RESP-IDARTNR-OBJ-UPPD = ALL '+'                                 
049802         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-OBJ-UPPD                  
049902       ELSE                                                               
050002         MOVE RESP-IDARTNR-OBJ-UPPD                                       
050102                                 TO MOD-IDARTNR-OBJ-UPPD                  
050202       END-IF                                                             
050302     END-IF                                                               
050402                                                                          
050502     MOVE RESP-KVRETUR-UPPD-ATTR TO MOD-KVRETUR-UPPD-ATTR                 
050602     IF RESP-KVRETUR-UPPD = SPACE                                         
050702       MOVE MFS-RENSA-FAELT      TO MOD-KVRETUR-UPPD                      
050802     ELSE                                                                 
050902       IF RESP-KVRETUR-UPPD = ALL '+'                                     
051002         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVRETUR-UPPD                      
051102       ELSE                                                               
051202         MOVE RESP-KVRETUR-UPPD  TO MOD-KVRETUR-UPPD                      
051302       END-IF                                                             
051402     END-IF                                                               
051502                                                                          
051602     MOVE RESP-FLBYTKNR-UPPD-ATTR                                         
051702                                 TO MOD-FLBYTKNR-UPPD-ATTR                
051802     IF RESP-FLBYTKNR-UPPD = SPACE                                        
051902       MOVE MFS-RENSA-FAELT      TO MOD-FLBYTKNR-UPPD                     
052002     ELSE                                                                 
052102       IF RESP-FLBYTKNR-UPPD = ALL '+'                                    
052202         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLBYTKNR-UPPD                     
052302       ELSE                                                               
052402         MOVE RESP-FLBYTKNR-UPPD TO MOD-FLBYTKNR-UPPD                     
052502       END-IF                                                             
052602     END-IF                                                               
052702                                                                          
052802     IF RESP-KVLS = SPACE                                                 
052902       MOVE MFS-RENSA-FAELT      TO MOD-KVLS                              
053002     ELSE                                                                 
053102       IF RESP-KVLS = ALL '+'                                             
053202         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVLS                              
053302       ELSE                                                               
053402         MOVE RESP-KVLS          TO MOD-KVLS                              
053502       END-IF                                                             
053602     END-IF                                                               
053702                                                                          
053802     IF RESP-IDBYTFAK = SPACE                                             
053902       MOVE MFS-RENSA-FAELT      TO MOD-IDBYTFAK                          
054002     ELSE                                                                 
054102       IF RESP-IDBYTFAK = ALL '+'                                         
054202         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDBYTFAK                          
054302       ELSE                                                               
054402         MOVE RESP-IDBYTFAK      TO MOD-IDBYTFAK                          
054502       END-IF                                                             
054602     END-IF                                                               
054702                                                                          
054802     MOVE RESP-IDARTNR-OBJ-BORT-ATTR                                      
054902                                 TO MOD-IDARTNR-OBJ-BORT-ATTR             
055002     IF RESP-IDARTNR-OBJ-BORT = SPACE                                     
055102       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-OBJ-BORT                  
055202     ELSE                                                                 
055302       IF RESP-IDARTNR-OBJ-BORT = ALL '+'                                 
055402         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-OBJ-BORT                  
055502       ELSE                                                               
055602         MOVE RESP-IDARTNR-OBJ-BORT                                       
055702                                 TO MOD-IDARTNR-OBJ-BORT                  
055802       END-IF                                                             
055902     END-IF                                                               
056002                                                                          
056102     MOVE RESP-KVRETUR-BORT-ATTR TO MOD-KVRETUR-BORT-ATTR                 
056202     IF RESP-KVRETUR-BORT = SPACE                                         
056302       MOVE MFS-RENSA-FAELT      TO MOD-KVRETUR-BORT                      
056402     ELSE                                                                 
056502       IF RESP-KVRETUR-BORT = ALL '+'                                     
056602         MOVE MFS-ROER-EJ-FAELT  TO MOD-KVRETUR-BORT                      
056702       ELSE                                                               
056802         MOVE RESP-KVRETUR-BORT  TO MOD-KVRETUR-BORT                      
056902       END-IF                                                             
057002     END-IF                                                               
057102                                                                          
057202     MOVE RESP-IDBYTKOL-BORT-ATTR                                         
057302                                 TO MOD-IDBYTKOL-BORT-ATTR                
057402     IF RESP-IDBYTKOL-BORT = SPACE                                        
057502       MOVE MFS-RENSA-FAELT      TO MOD-IDBYTKOL-BORT                     
057602     ELSE                                                                 
057702       IF RESP-IDBYTKOL-BORT = ALL '+'                                    
057802         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDBYTKOL-BORT                     
057902       ELSE                                                               
058002         MOVE RESP-IDBYTKOL-BORT TO MOD-IDBYTKOL-BORT                     
058102       END-IF                                                             
058202     END-IF                                                               
058302                                                                          
058402     MOVE RESP-VKORDBTO-FAKT-UPD-ATTR                                     
058502                                 TO MOD-VKORDBTO-FAKT-IN-ATTR             
058602     IF RESP-VKORDBTO-FAKT-UPD = SPACE                                    
058702       MOVE MFS-RENSA-FAELT      TO MOD-VKORDBTO-FAKT-IN                  
058802     ELSE                                                                 
058902       IF RESP-VKORDBTO-FAKT-UPD = ALL '+'                                
059002         MOVE MFS-ROER-EJ-FAELT  TO MOD-VKORDBTO-FAKT-IN                  
059102       ELSE                                                               
059202         MOVE RESP-VKORDBTO-FAKT-UPD                                      
059302                                 TO MOD-VKORDBTO-FAKT-IN                  
059402       END-IF                                                             
059502     END-IF                                                               
059602                                                                          
059702     IF RESP-VKORDBTO-FAKT-UT = SPACE                                     
059802       MOVE MFS-RENSA-FAELT      TO MOD-VKORDBTO-FAKT-UT                  
059902     ELSE                                                                 
060002       IF RESP-VKORDBTO-FAKT-UT = ALL '+'                                 
060102         MOVE MFS-ROER-EJ-FAELT  TO MOD-VKORDBTO-FAKT-UT                  
060202       ELSE                                                               
060302         MOVE RESP-VKORDBTO-FAKT-UT                                       
060402                                 TO MOD-VKORDBTO-FAKT-UT                  
060502       END-IF                                                             
060602     END-IF                                                               
060702                                                                          
060802     MOVE RESP-VLORDBTO-FAKT-UPD-ATTR                                     
060902                                 TO MOD-VLORDBTO-FAKT-IN-ATTR             
061002     IF RESP-VLORDBTO-FAKT-UPD = SPACE                                    
061102       MOVE MFS-RENSA-FAELT      TO MOD-VLORDBTO-FAKT-IN                  
061202     ELSE                                                                 
061302       IF RESP-VLORDBTO-FAKT-UPD = ALL '+'                                
061402         MOVE MFS-ROER-EJ-FAELT  TO MOD-VLORDBTO-FAKT-IN                  
061502       ELSE                                                               
061602         MOVE RESP-VLORDBTO-FAKT-UPD                                      
061702                                 TO MOD-VLORDBTO-FAKT-IN                  
061802       END-IF                                                             
061902     END-IF                                                               
062002                                                                          
062102     IF RESP-VLORDBTO-FAKT-UT = SPACE                                     
062202       MOVE MFS-RENSA-FAELT      TO MOD-VLORDBTO-FAKT-UT                  
062302     ELSE                                                                 
062402       IF RESP-VLORDBTO-FAKT-UT = ALL '+'                                 
062502         MOVE MFS-ROER-EJ-FAELT  TO MOD-VLORDBTO-FAKT-UT                  
062602       ELSE                                                               
062702         MOVE RESP-VLORDBTO-FAKT-UT                                       
062802                                 TO MOD-VLORDBTO-FAKT-UT                  
062902       END-IF                                                             
063002     END-IF                                                               
063102                                                                          
063202     MOVE RESP-GODK-FAKT-ATTR    TO MOD-GODK-FAKT-ATTR                    
063302     IF RESP-GODK-FAKT = SPACE                                            
063402       MOVE MFS-RENSA-FAELT      TO MOD-GODK-FAKT                         
063502     ELSE                                                                 
063602       IF RESP-GODK-FAKT = ALL '+'                                        
063702         MOVE MFS-ROER-EJ-FAELT  TO MOD-GODK-FAKT                         
063802       ELSE                                                               
063902         MOVE RESP-GODK-FAKT     TO MOD-GODK-FAKT                         
064002       END-IF                                                             
064102     END-IF                                                               
064202                                                                          
064302     PERFORM                                                              
064402     VARYING INDX FROM +1 BY +1                                           
064502       UNTIL INDX > RESP-KVRADER                                          
064602                                                                          
064702       IF RESP-IDARTNR-OBJ-LINE (INDX) = SPACE                            
064802         MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-OBJ (INDX)                
064902       ELSE                                                               
065002         IF RESP-IDARTNR-OBJ-LINE (INDX) = ALL '+'                        
065102           MOVE MFS-ROER-EJ-FAELT                                         
065202                                 TO MOD-IDARTNR-OBJ (INDX)                
065302         ELSE                                                             
065402           MOVE RESP-IDARTNR-OBJ-LINE (INDX)                              
065502                                 TO MOD-IDARTNR-OBJ (INDX)                
065602         END-IF                                                           
065702       END-IF                                                             
065802                                                                          
065902       IF RESP-KVRETUR-LINE (INDX) = SPACE                                
066002         MOVE MFS-RENSA-FAELT    TO MOD-KVRETUR (INDX)                    
066102       ELSE                                                               
066202         IF RESP-KVRETUR-LINE (INDX) = ALL '+'                            
066302           MOVE MFS-ROER-EJ-FAELT                                         
066402                                 TO MOD-KVRETUR (INDX)                    
066502         ELSE                                                             
066602           MOVE RESP-KVRETUR-LINE (INDX)                                  
066702                                 TO MOD-KVRETUR (INDX)                    
066802         END-IF                                                           
066902       END-IF                                                             
067002                                                                          
067102       IF RESP-IDBYTKOL-LINE (INDX) = SPACE                               
067202         MOVE MFS-RENSA-FAELT    TO MOD-IDBYTKOL (INDX)                   
067302       ELSE                                                               
067402         IF RESP-IDBYTKOL-LINE (INDX) = ALL '+'                           
067502           MOVE MFS-ROER-EJ-FAELT                                         
067602                                 TO MOD-IDBYTKOL (INDX)                   
067702         ELSE                                                             
067802           MOVE RESP-IDBYTKOL-LINE (INDX)                                 
067902                                 TO MOD-IDBYTKOL (INDX)                   
068002         END-IF                                                           
068102       END-IF                                                             
068202                                                                          
068302       IF RESP-BEART-LINE (INDX) = SPACE                                  
068402         MOVE MFS-RENSA-FAELT    TO MOD-BEART (INDX)                      
068502       ELSE                                                               
068602         IF RESP-BEART-LINE (INDX) = ALL '+'                              
068702           MOVE MFS-ROER-EJ-FAELT                                         
068802                                 TO MOD-BEART (INDX)                      
068902         ELSE                                                             
069002           MOVE RESP-BEART-LINE (INDX)                                    
069102                                 TO MOD-BEART (INDX)                      
069202         END-IF                                                           
069302       END-IF                                                             
069402                                                                          
069502     END-PERFORM                                                          
069602                                                                          
069702     PERFORM                                                              
069802     VARYING INDX FROM INDX BY +1                                         
069902       UNTIL INDX > MAX-INDX                                              
070002       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-OBJ (INDX)                
070102                                    MOD-KVRETUR     (INDX)                
070202                                    MOD-IDBYTKOL    (INDX)                
070302                                    MOD-BEART       (INDX)                
070402     END-PERFORM                                                          
070502                                                                          
070602     .                                                                    
070702     EJECT                                                                
070802 MFS-RENSA-FAELT-IN SECTION.                                              
070902     MOVE 'MFS-RENSA-FAELT-IN'   TO WS-SEKTION                            
071002                                                                          
071102     MOVE +1 TO INDX                                                      
071202     PERFORM UNTIL INDX > MAX-INDX                                        
071302       MOVE MFS-RENSA-FAELT      TO MOD-KVRETUR     (INDX)                
071402                                    MOD-IDBYTKOL    (INDX)                
071502                                    MOD-IDARTNR-OBJ (INDX)                
071602                                    MOD-BEART       (INDX)                
071702       ADD +1                    TO INDX                                  
071802     END-PERFORM                                                          
071902                                                                          
072002     MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-OBJ-UPPD                  
072102                                    MOD-KVRETUR-UPPD                      
072202                                    MOD-FLBYTKNR-UPPD                     
072302                                    MOD-KVLS                              
072402                                    MOD-IDBYTFAK                          
072502                                    MOD-GODK-FAKT                         
072602                                    MOD-IDARTNR-OBJ-BORT                  
072702                                    MOD-KVRETUR-BORT                      
072802                                    MOD-IDBYTKOL-BORT                     
072902                                    MOD-VKORDBTO-FAKT-IN                  
073002                                    MOD-VLORDBTO-FAKT-IN                  
073102     .                                                                    
073202     EJECT                                                                
073302 MFS-FORM-ATTR SECTION.                                                   
073402     MOVE 'MFS-FORM-ATTR'        TO WS-SEKTION                            
073502                                                                          
073602*    --- ALLA INDATA-FÄLT                                                 
073702                                                                          
073802     MOVE MFS-FORMATETS-ATTR     TO MOD-IDARTNR-OBJ-BORT-ATTR             
073902                                    MOD-KVRETUR-BORT-ATTR                 
074002                                    MOD-IDBYTKOL-BORT-ATTR                
074102                                    MOD-GODK-FAKT-ATTR                    
074202                                    MOD-IDARTNR-OBJ-UPPD-ATTR             
074302                                    MOD-KVRETUR-UPPD-ATTR                 
074402                                    MOD-FLBYTKNR-UPPD-ATTR                
074502                                    MOD-VKORDBTO-FAKT-IN-ATTR             
074602                                    MOD-VLORDBTO-FAKT-IN-ATTR             
074702     .                                                                    
074802     SKIP2                                                                
074902* --- IMS SEKTIONER ---                                                   
075002     SKIP3                                                                
075102 IMS-GET-MSG SECTION.                                                     
075202     MOVE 'IMS-GET-MSG'          TO WS-IMS-SEKTION                        
075302                                                                          
075402     MOVE '  QC' TO GODK-STATUSKODER                                      
075502     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
075602     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075702     PERFORM IMS-STATUSKONTROLL                                           
075802     .                                                                    
075902     SKIP3                                                                
076002 IMS-INSERT-MSG SECTION.                                                  
076102     MOVE 'IMS-INSERT-MSG'       TO WS-IMS-SEKTION                        
076202                                                                          
076302     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
076402       MOVE '0' TO MFS-KDHUVOMR                                           
076502     END-IF                                                               
076602     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
076702     MOVE SPACE TO GODK-STATUSKODER                                       
076802     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076902     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077002     PERFORM IMS-STATUSKONTROLL                                           
077102     .                                                                    
077202     EJECT                                                                
077302 IMS-STATUSKONTROLL SECTION.                                              
077402                                                                          
077502     SET STATUS-IX TO 1                                                   
077602     SEARCH GODK-STATUS                                                   
077702       AT END                                                             
077802         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
077902         DELIMITED BY SIZE INTO FELTEXT                                   
078002         CALL FELLOG                                                      
078102       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
078202         CONTINUE                                                         
078302     END-SEARCH                                                           
078402     .                                                                    
079002     EJECT                                                                
