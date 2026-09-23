000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2011600.                                                
000300 AUTHOR.         KENT JEBSEN.                                             
000400 DATE-WRITTEN.   96/11/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING AV LEVERANTÖRSUPPGIFTER USA                          
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDF1                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W2T116                                              
001400*        MID:         W2I11601                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W2O11601                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W2011600'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300                                                                          
003401 77   TAB-IX1               PIC S9(9)  COMP SYNC.                         
003402 77   TAB-IX1-MAX           PIC S9(9)  VALUE +5 COMP SYNC.                
003403 77   TAB-IX2               PIC S9(9)  COMP SYNC.                         
003404 77   TAB-IX2-MAX           PIC S9(9)  VALUE +8 COMP SYNC.                
003405                                                                          
003406*01 -COPY WWDCKONS                                                        
003502                                                                          
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700                                                                          
003800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003900     88  INDATA-OK                           VALUE 'J'.                   
004000     88  INDATA-FEL                          VALUE 'N'.                   
004100                                                                          
004200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004300     88  NYCKLAR-OK                          VALUE 'J'.                   
004400     88  NYCKLAR-FEL                         VALUE 'N'.                   
004500                                                                          
004600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004700     88  EGEN-MID                            VALUE '2116'.                
004800     88  GODK-MID                            VALUE '2111' '2112'          
004900                                                   '2113' '2114'          
005000                                                   '2115' '2116'          
005100                                                   '2117' '2118'          
005200                                                   '2119'.                
005300     88  HELP-MID                            VALUE '0551'.                
005400     EJECT                                                                
005500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005600 01  GENERELLA-SUBPROGRAM.                                                
005700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     EJECT                                                                
006200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006300*01 -COPY WMEDAREA                                                        
006400     SKIP3                                                                
006500 01  MESSAGE-CODES.                                                       
006600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007100     03  ERR-WRONG-SUPPL         PIC X(3)    VALUE '092'.                 
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007400*                                                                         
007500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007600     SKIP3                                                                
007700*01 -COPY WMSGINIT                                                        
007800     SKIP3                                                                
007900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008000*                                                                         
008100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008200     SKIP3                                                                
008300*01  MID -COPY W2I11601                                                   
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008600     SKIP3                                                                
008700*01  -COPY WMSGAREA                                                       
008800     EJECT                                                                
008900     03  MOD REDEFINES MSG-AREA.                                          
009000*      05  -COPY W2O11601                                                 
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009300     SKIP3                                                                
009400*01  -COPY WMFSAREA                                                       
009500     EJECT                                                                
009600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009700*                                                                         
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP3                                                                
010000 01  NYCKLAR-TILL-DLI.                                                    
010100     03  W-IDLEVNR-X.                                                     
010200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
010300     03  W-IDLEVSUF-X.                                                    
010400         05  W-IDLEVSUF          PIC S9(1)   VALUE ZERO COMP-3.           
010500     03  W-IDDC-X.                                                        
010600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011904 01  SSA3                        PIC X(64).                               
012004     EJECT                                                                
012104*    --- IMS FUNKTIONSKODER                                               
012204*01  -COPY W0003                                                          
012304     EJECT                                                                
012404*    ---  DLI INPUT-OUTPUT AREA                                           
012504                                                                          
012604 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF101'.                      
012704 01  DLI-IO-WDF101.                                                       
012804*    03  -COPY WDF101  -PRE WDF1-                                         
012904     EJECT                                                                
013004 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF106'.                      
013104 01  DLI-IO-WDF106.                                                       
013204*    03  -COPY WDF106  -PRE WDF1-                                         
013304     EJECT                                                                
013404 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF116'.                      
013504 01  DLI-IO-WDF116.                                                       
013604*    03  -COPY WDF116  -PRE WDF1-                                         
013704     EJECT                                                                
013804 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF121'.                      
013904 01  DLI-IO-WDF121.                                                       
014004*    03  -COPY WDF121  -PRE WDF1-                                         
014104     EJECT                                                                
014204 LINKAGE SECTION.                                                         
014304*01  -COPY W0009   -PRE MSG-                                              
014404*01  -COPY W0008   -PRE USEA-                                             
014504     05  FILLER                  PIC X.                                   
014604     EJECT                                                                
014704*01  -COPY W0008  -PRE WDF1-                                              
014804     05  FILLER                  PIC X.                                   
014904     EJECT                                                                
015004 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDF1-PCB.                     
015104 MAIN SECTION.                                                            
015204     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDF1-PCB.                     
015304                                                                          
015404     PERFORM IMS-GET-MSG                                                  
015504     IF SEGMENT-FINNS                                                     
015604       PERFORM A-INIT                                                     
015704       PERFORM B-KOLLA-NYCKLAR                                            
015804       IF NYCKLAR-OK                                                      
015904         IF MFS-UPDATE                                                    
016004           PERFORM G-KOLLA-INPUT                                          
016104           IF INDATA-OK                                                   
016204             PERFORM H-UPPDATERA                                          
016304           END-IF                                                         
016404         ELSE                                                             
016504           IF MFS-FIRST                                                   
016604             PERFORM C-FOERSTA-SIDA                                       
016704           ELSE                                                           
016804             PERFORM E-SAMMA-SIDA                                         
016904           END-IF                                                         
017004         END-IF                                                           
017104         PERFORM F-LAES-VISA-INFO                                         
017204       END-IF                                                             
017304       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O11601 + 4                      
017404       PERFORM IMS-INSERT-MSG                                             
017504     END-IF                                                               
017604                                                                          
017704     MOVE ZERO TO RETURN-CODE                                             
017804     GOBACK                                                               
017904     .                                                                    
018004     EJECT                                                                
018104 A-INIT SECTION.                                                          
018204                                                                          
018304     IF MSG-DUBBLA-TRANSKODER                                             
018404       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I11601                 
018504       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018604       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018704     ELSE                                                                 
018804       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I11601                  
018904       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
019004       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019104     END-IF                                                               
019204                                                                          
019304     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
019404     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
019504     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019604                                                                          
019704     MOVE LOW-VALUE TO MSG-AREA                                           
019804     MOVE 'W2O116N1' TO MFS-IDMOD                                         
019904     MOVE '2116' TO MOD-IDTRANS                                           
020004     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
020104                                                                          
020204     IF EGEN-MID OR HELP-MID                                              
020304       CONTINUE                                                           
020404     ELSE                                                                 
020504       MOVE SPACE TO MFS-KDTRTYP                                          
020604       MOVE '7' TO MFS-IDPFK                                              
020704     END-IF                                                               
020804     .                                                                    
020904     EJECT                                                                
021004 B-KOLLA-NYCKLAR SECTION.                                                 
021104                                                                          
021204     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021304     MOVE '001'             TO MSGI-KDCALL                                
021404     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021504     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021604     MOVE '2116'            TO MSGI-IDTRANS                               
021704     IF GODK-MID                                                          
021804         MOVE MID-IDLEVNR-IN     TO MSGI-IDLEVNR                          
021904     END-IF                                                               
022004     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022104     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
022204                                                                          
022304     MOVE JA TO NYCKLAR-SW                                                
022404                                                                          
022504                                                                          
022604*    -- KONTROLL AV IDLEVNR                                               
022704     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
022804                                                                          
022904     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
023004       MOVE '7'         TO MFS-IDPFK                                      
023104       MOVE SPACE       TO MFS-KDTRTYP                                    
023204     END-IF                                                               
023304                                                                          
023404     IF MSGI-IDLEVNR NOT = SPACE                                          
023504       MOVE MSGI-IDLEVNR TO W-IDLEVNR                                     
023604     ELSE                                                                 
023704       MOVE NEJ TO NYCKLAR-SW                                             
023804     END-IF                                                               
023904                                                                          
024004     IF GODK-MID OR NYCKLAR-OK                                            
024104       MOVE MSGI-IDLEVNR        TO MOD-IDLEVNR-UT                         
024204     ELSE                                                                 
024304       MOVE MFS-RENSA-FAELT     TO MOD-IDLEVNR-UT                         
024404     END-IF                                                               
024504                                                                          
024604     IF NYCKLAR-FEL                                                       
024704       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
024804       CALL WMEDKONV USING MED-WMEDAREA                                   
024904       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025004       PERFORM MFS-RENSA-FAELT-IN                                         
025104       PERFORM MFS-RENSA-FAELT-UT                                         
025204     END-IF                                                               
025304     .                                                                    
025404     EJECT                                                                
025504 C-FOERSTA-SIDA SECTION.                                                  
025604                                                                          
025704     PERFORM MFS-RENSA-FAELT-IN                                           
025804     .                                                                    
025904     EJECT                                                                
026004 E-SAMMA-SIDA SECTION.                                                    
026104                                                                          
026204     IF EGEN-MID OR HELP-MID                                              
026304        MOVE INF-PRESS-PF11 TO MED-IDMFSINF                               
026404        CALL WMEDKONV USING MED-WMEDAREA                                  
026504        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
026604        PERFORM EA-MID-INDATA-TILL-MOD                                    
026704     ELSE                                                                 
026804       PERFORM MFS-RENSA-FAELT-IN                                         
026904     END-IF                                                               
027004     .                                                                    
027104     EJECT                                                                
027204 EA-MID-INDATA-TILL-MOD SECTION.                                          
027304                                                                          
027404     INSPECT MID-IDDC51-IN REPLACING LEADING ZERO BY SPACE                
027504     IF MID-IDDC51-IN NOT = ALL '+'                                       
027604        MOVE MID-IDDC51-IN         TO MOD-IDDC51-IN                       
027704        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC51-IN-ATTR                  
027804     ELSE                                                                 
027904        MOVE MFS-RENSA-FAELT       TO MOD-IDDC51-IN                       
028004     END-IF                                                               
028104                                                                          
028105     INSPECT MID-IDDC52-IN REPLACING LEADING ZERO BY SPACE                
028106     IF MID-IDDC52-IN NOT = ALL '+'                                       
028107        MOVE MID-IDDC52-IN         TO MOD-IDDC52-IN                       
028108        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC52-IN-ATTR                  
028109     ELSE                                                                 
028110        MOVE MFS-RENSA-FAELT       TO MOD-IDDC52-IN                       
028120     END-IF                                                               
028130                                                                          
028140     INSPECT MID-IDDC53-IN REPLACING LEADING ZERO BY SPACE                
028150     IF MID-IDDC53-IN NOT = ALL '+'                                       
028160        MOVE MID-IDDC53-IN         TO MOD-IDDC53-IN                       
028170        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC53-IN-ATTR                  
028180     ELSE                                                                 
028190        MOVE MFS-RENSA-FAELT       TO MOD-IDDC53-IN                       
028200     END-IF                                                               
028201                                                                          
028204     INSPECT MID-IDDC61-IN REPLACING LEADING ZERO BY SPACE                
028304     IF MID-IDDC61-IN NOT = ALL '+'                                       
028404        MOVE MID-IDDC61-IN         TO MOD-IDDC61-IN                       
028504        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC61-IN-ATTR                  
028604     ELSE                                                                 
028704        MOVE MFS-RENSA-FAELT       TO MOD-IDDC61-IN                       
028804     END-IF                                                               
028904                                                                          
029004     INSPECT MID-IDDC62-IN REPLACING LEADING ZERO BY SPACE                
029104     IF MID-IDDC62-IN NOT = ALL '+'                                       
029204        MOVE MID-IDDC62-IN         TO MOD-IDDC62-IN                       
029304        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC62-IN-ATTR                  
029404     ELSE                                                                 
029504        MOVE MFS-RENSA-FAELT       TO MOD-IDDC62-IN                       
029604     END-IF                                                               
029704                                                                          
029804     INSPECT MID-IDDC63-IN REPLACING LEADING ZERO BY SPACE                
029904     IF MID-IDDC63-IN NOT = ALL '+'                                       
030004        MOVE MID-IDDC63-IN         TO MOD-IDDC63-IN                       
030104        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC63-IN-ATTR                  
030204     ELSE                                                                 
030304        MOVE MFS-RENSA-FAELT       TO MOD-IDDC63-IN                       
030404     END-IF                                                               
030504                                                                          
030604     INSPECT MID-IDDC64-IN REPLACING LEADING ZERO BY SPACE                
030704     IF MID-IDDC64-IN NOT = ALL '+'                                       
030804        MOVE MID-IDDC64-IN         TO MOD-IDDC64-IN                       
030904        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC64-IN-ATTR                  
031004     ELSE                                                                 
031104        MOVE MFS-RENSA-FAELT       TO MOD-IDDC64-IN                       
031204     END-IF                                                               
031304                                                                          
031404     INSPECT MID-IDDC65-IN REPLACING LEADING ZERO BY SPACE                
031504     IF MID-IDDC65-IN NOT = ALL '+'                                       
031604        MOVE MID-IDDC65-IN         TO MOD-IDDC65-IN                       
031704        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC65-IN-ATTR                  
031804     ELSE                                                                 
031904        MOVE MFS-RENSA-FAELT       TO MOD-IDDC65-IN                       
032004     END-IF                                                               
032104                                                                          
032105     INSPECT MID-IDDC66-IN REPLACING LEADING ZERO BY SPACE                
032106     IF MID-IDDC66-IN NOT = ALL '+'                                       
032107        MOVE MID-IDDC66-IN         TO MOD-IDDC66-IN                       
032108        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC66-IN-ATTR                  
032109     ELSE                                                                 
032110        MOVE MFS-RENSA-FAELT       TO MOD-IDDC66-IN                       
032111     END-IF                                                               
032112                                                                          
032113     INSPECT MID-IDDC67-IN REPLACING LEADING ZERO BY SPACE                
032114     IF MID-IDDC67-IN NOT = ALL '+'                                       
032115        MOVE MID-IDDC67-IN         TO MOD-IDDC67-IN                       
032116        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC67-IN-ATTR                  
032117     ELSE                                                                 
032118        MOVE MFS-RENSA-FAELT       TO MOD-IDDC67-IN                       
032120     END-IF                                                               
032130                                                                          
032140     INSPECT MID-IDDC87-IN REPLACING LEADING ZERO BY SPACE                
032150     IF MID-IDDC87-IN NOT = ALL '+'                                       
032160        MOVE MID-IDDC87-IN         TO MOD-IDDC87-IN                       
032170        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC87-IN-ATTR                  
032180     ELSE                                                                 
032190        MOVE MFS-RENSA-FAELT       TO MOD-IDDC87-IN                       
032200     END-IF                                                               
032300                                                                          
034604     IF MID-TEFRAKT NOT = ALL '+'                                         
034704        MOVE MID-TEFRAKT           TO MOD-TEFRAKT                         
034804        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEFRAKT-ATTR                    
034904     ELSE                                                                 
035004        MOVE MFS-RENSA-FAELT       TO MOD-TEFRAKT                         
035104     END-IF                                                               
035204                                                                          
035304     IF MID-TEFRAVIL NOT = ALL '+'                                        
035404        MOVE MID-TEFRAVIL          TO MOD-TEFRAVIL                        
035504        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEFRAVIL-ATTR                   
035604     ELSE                                                                 
035704        MOVE MFS-RENSA-FAELT       TO MOD-TEFRAVIL                        
035804     END-IF                                                               
035904                                                                          
036004     IF MID-TETERMS NOT = ALL '+'                                         
036104        MOVE MID-TETERMS           TO MOD-TETERMS                         
036204        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TETERMS-ATTR                    
036304     ELSE                                                                 
036404        MOVE MFS-RENSA-FAELT       TO MOD-TETERMS                         
036504     END-IF                                                               
036604     .                                                                    
036704     EJECT                                                                
036804 F-LAES-VISA-INFO SECTION.                                                
036904                                                                          
037004     PERFORM IMS-GET-WDF1-LEV                                             
037104                                                                          
037204     IF SEGMENT-FINNS                                                     
037304        PERFORM FA-LAES-GRUNDDATA                                         
037404     ELSE                                                                 
037504        MOVE ERR-WRONG-SUPPL TO MED-IDMFSFEL                              
037604        CALL WMEDKONV USING MED-WMEDAREA                                  
037704        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
037804        PERFORM MFS-RENSA-FAELT-UT                                        
037904        PERFORM MFS-STAENG-FAELT-IN                                       
038004     END-IF                                                               
038104     .                                                                    
038204     EJECT                                                                
038304 FA-LAES-GRUNDDATA SECTION.                                               
038404                                                                          
038800     PERFORM IMS-GET-WDF1-ADR                                             
038900                                                                          
039000     IF SEGMENT-FINNS                                                     
039100        MOVE WDF1-ADR-BELEV            TO MOD-BELEV                       
039200        MOVE WDF1-ADR-ADLEV-RAD1       TO MOD-ADLEV-RAD1                  
039300        MOVE WDF1-ADR-ADLEV-RAD2       TO MOD-ADLEV-RAD2                  
039400        MOVE WDF1-ADR-ADLEV-ORT        TO MOD-ADLEV-ORT                   
039500        MOVE WDF1-ADR-ADLEVLND         TO MOD-ADLEVLND                    
039600     END-IF                                                               
039700                                                                          
039802     MOVE WC-NDC-CA     TO W-IDDC                                         
039900     PERFORM IMS-GET-WDF1-NDC                                             
040000                                                                          
040100     IF SEGMENT-FINNS                                                     
040200                                                                          
040303        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC51-UT                
040402        IF INDATA-FEL OR MFS-ENTER                                        
040502           IF MID-TEFRAKT NOT = ALL '+'                                   
040602              MOVE MID-TEFRAKT            TO MOD-TEFRAKT                  
040702           ELSE                                                           
040802              MOVE SPACE                  TO MOD-TEFRAKT                  
040902           END-IF                                                         
041002           IF MID-TEFRAVIL NOT = ALL '+'                                  
041102              MOVE MID-TEFRAVIL           TO MOD-TEFRAVIL                 
041202           ELSE                                                           
041302              MOVE SPACE                  TO MOD-TEFRAVIL                 
041402           END-IF                                                         
041502           IF MID-TETERMS NOT = ALL '+'                                   
041602              MOVE MID-TETERMS            TO MOD-TETERMS                  
041702           ELSE                                                           
041802              MOVE SPACE                  TO MOD-TETERMS                  
041902           END-IF                                                         
042002        ELSE                                                              
042102           PERFORM FAA-GET-WDF1-TXT                                       
042202        END-IF                                                            
042302                                                                          
042402     END-IF                                                               
042403                                                                          
042404     MOVE WC-NDC-BR     TO W-IDDC                                         
042405     PERFORM IMS-GET-WDF1-NDC                                             
042406     IF SEGMENT-FINNS                                                     
042407        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC52-UT                
042408     END-IF                                                               
042409                                                                          
042410     MOVE WC-NDC-MX     TO W-IDDC                                         
042420     PERFORM IMS-GET-WDF1-NDC                                             
042430     IF SEGMENT-FINNS                                                     
042440        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC53-UT                
042450     END-IF                                                               
042502                                                                          
042602     MOVE WC-NDC-JP     TO W-IDDC                                         
042702     PERFORM IMS-GET-WDF1-NDC                                             
042902     IF SEGMENT-FINNS                                                     
043103        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC61-UT                
045202     END-IF                                                               
045302                                                                          
045402     MOVE WC-NDC-AU     TO W-IDDC                                         
045502     PERFORM IMS-GET-WDF1-NDC                                             
045602     IF SEGMENT-FINNS                                                     
045703        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC62-UT                
045802     END-IF                                                               
045902                                                                          
046002     MOVE WC-NDC-TH     TO W-IDDC                                         
046102     PERFORM IMS-GET-WDF1-NDC                                             
046202     IF SEGMENT-FINNS                                                     
046303        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC63-UT                
046402     END-IF                                                               
046502                                                                          
046602     MOVE WC-NDC-TW     TO W-IDDC                                         
046702     PERFORM IMS-GET-WDF1-NDC                                             
046802     IF SEGMENT-FINNS                                                     
046903        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC64-UT                
047002     END-IF                                                               
047102                                                                          
047202     MOVE WC-NDC-KR     TO W-IDDC                                         
047302     PERFORM IMS-GET-WDF1-NDC                                             
047402     IF SEGMENT-FINNS                                                     
047503        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC65-UT                
047602     END-IF                                                               
047702                                                                          
047703     MOVE WC-NDC-MY     TO W-IDDC                                         
047704     PERFORM IMS-GET-WDF1-NDC                                             
047705     IF SEGMENT-FINNS                                                     
047706        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC66-UT                
047707     END-IF                                                               
047708                                                                          
047802     MOVE WC-NDC-IN     TO W-IDDC                                         
047902     PERFORM IMS-GET-WDF1-NDC                                             
048002     IF SEGMENT-FINNS                                                     
048103        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC67-UT                
048202     END-IF                                                               
048203                                                                          
048204     MOVE WC-NDC-ZA     TO W-IDDC                                         
048205     PERFORM IMS-GET-WDF1-NDC                                             
048206     IF SEGMENT-FINNS                                                     
048207        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC85-UT                
048208     END-IF                                                               
048209                                                                          
048210     MOVE WC-NDC-TR     TO W-IDDC                                         
048211     PERFORM IMS-GET-WDF1-NDC                                             
048212     IF SEGMENT-FINNS                                                     
048213        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC86-UT                
048214     END-IF                                                               
048215                                                                          
048216     MOVE WC-NDC-AE     TO W-IDDC                                         
048217     PERFORM IMS-GET-WDF1-NDC                                             
048218     IF SEGMENT-FINNS                                                     
048219        MOVE WDF1-NDC-KVDAGAR-TBT         TO MOD-IDDC87-UT                
048220     END-IF                                                               
050002     .                                                                    
050102     EJECT                                                                
050202 FAA-GET-WDF1-TXT  SECTION.                                               
050302                                                                          
050402     PERFORM IMS-GET-WDF1-TXT                                             
050502                                                                          
050602     IF SEGMENT-FINNS                                                     
050702                                                                          
050802        MOVE WDF1-TXT-TEFRAKT             TO MOD-TEFRAKT                  
050902        IF MOD-TEFRAKT = ALL '+'                                          
051002           MOVE SPACE                     TO MOD-TEFRAKT                  
051102        END-IF                                                            
051202        MOVE WDF1-TXT-TEFRAVIL            TO MOD-TEFRAVIL                 
051302        IF MOD-TEFRAVIL = ALL '+'                                         
051402           MOVE SPACE                     TO MOD-TEFRAVIL                 
051502        END-IF                                                            
051602        MOVE WDF1-TXT-TETERMS             TO MOD-TETERMS                  
051702        IF MOD-TETERMS = ALL '+'                                          
051802           MOVE SPACE                     TO MOD-TETERMS                  
051902        END-IF                                                            
052002                                                                          
052102     ELSE                                                                 
052202        IF MID-TEFRAKT NOT = ALL '+'                                      
052302           MOVE MID-TEFRAKT               TO MOD-TEFRAKT                  
052402        ELSE                                                              
052502           MOVE SPACE                     TO MOD-TEFRAKT                  
052602        END-IF                                                            
052702        IF MID-TEFRAVIL NOT = ALL '+'                                     
052802           MOVE MID-TEFRAVIL              TO MOD-TEFRAVIL                 
052902        ELSE                                                              
053002           MOVE SPACE                     TO MOD-TEFRAVIL                 
053102        END-IF                                                            
053202        IF MID-TETERMS NOT = ALL '+'                                      
053302           MOVE MID-TETERMS               TO MOD-TETERMS                  
053402        ELSE                                                              
053502           MOVE SPACE                     TO MOD-TETERMS                  
053602        END-IF                                                            
053702     END-IF                                                               
053802     .                                                                    
053902     EJECT                                                                
054002 G-KOLLA-INPUT SECTION.                                                   
054102                                                                          
054202     MOVE JA  TO INDATA-SW                                                
054302                                                                          
054402     INSPECT MID-IDDC51-IN REPLACING LEADING SPACE BY ZERO                
054502     IF MID-IDDC51-IN NOT = ALL '+'                                       
054602       IF MID-IDDC51-IN NOT NUMERIC                                       
054702         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC51-IN-ATTR                     
054802         MOVE NEJ TO INDATA-SW                                            
054902       ELSE                                                               
055002         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC51-IN-ATTR                   
055102       END-IF                                                             
055202     ELSE                                                                 
055302       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC51-IN-ATTR                     
055402     END-IF                                                               
055502                                                                          
055503     INSPECT MID-IDDC52-IN REPLACING LEADING SPACE BY ZERO                
055504     IF MID-IDDC52-IN NOT = ALL '+'                                       
055505       IF MID-IDDC52-IN NOT NUMERIC                                       
055506         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC52-IN-ATTR                     
055507         MOVE NEJ TO INDATA-SW                                            
055508       ELSE                                                               
055509         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC52-IN-ATTR                   
055510       END-IF                                                             
055520     ELSE                                                                 
055530       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC52-IN-ATTR                     
055540     END-IF                                                               
055550                                                                          
055560     INSPECT MID-IDDC53-IN REPLACING LEADING SPACE BY ZERO                
055570     IF MID-IDDC53-IN NOT = ALL '+'                                       
055580       IF MID-IDDC53-IN NOT NUMERIC                                       
055590         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC53-IN-ATTR                     
055600         MOVE NEJ TO INDATA-SW                                            
055700       ELSE                                                               
055800         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC53-IN-ATTR                   
055900       END-IF                                                             
056000     ELSE                                                                 
056100       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC53-IN-ATTR                     
056200     END-IF                                                               
056300                                                                          
059202     INSPECT MID-IDDC61-IN REPLACING LEADING SPACE BY ZERO                
059302     IF MID-IDDC61-IN NOT = ALL '+'                                       
059402       IF MID-IDDC61-IN NOT NUMERIC                                       
059502         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC61-IN-ATTR                     
059602         MOVE NEJ TO INDATA-SW                                            
059702       ELSE                                                               
059802         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC61-IN-ATTR                   
059902       END-IF                                                             
060002     ELSE                                                                 
060102       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC61-IN-ATTR                     
060202     END-IF                                                               
060302                                                                          
060402     INSPECT MID-IDDC62-IN REPLACING LEADING SPACE BY ZERO                
060502     IF MID-IDDC62-IN NOT = ALL '+'                                       
060602       IF MID-IDDC62-IN NOT NUMERIC                                       
060702         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC62-IN-ATTR                     
060802         MOVE NEJ TO INDATA-SW                                            
060902       ELSE                                                               
061002         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC62-IN-ATTR                   
061102       END-IF                                                             
061202     ELSE                                                                 
061302       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC62-IN-ATTR                     
061402     END-IF                                                               
061502                                                                          
061503     INSPECT MID-IDDC63-IN REPLACING LEADING SPACE BY ZERO                
061504     IF MID-IDDC63-IN NOT = ALL '+'                                       
061505       IF MID-IDDC63-IN NOT NUMERIC                                       
061506         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC63-IN-ATTR                     
061507         MOVE NEJ TO INDATA-SW                                            
061508       ELSE                                                               
061509         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC63-IN-ATTR                   
061510       END-IF                                                             
061511     ELSE                                                                 
061512       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC63-IN-ATTR                     
061513     END-IF                                                               
061514                                                                          
061515     INSPECT MID-IDDC64-IN REPLACING LEADING SPACE BY ZERO                
061516     IF MID-IDDC64-IN NOT = ALL '+'                                       
061517       IF MID-IDDC64-IN NOT NUMERIC                                       
061518         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC64-IN-ATTR                     
061519         MOVE NEJ TO INDATA-SW                                            
061520       ELSE                                                               
061521         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC64-IN-ATTR                   
061522       END-IF                                                             
061523     ELSE                                                                 
061524       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC64-IN-ATTR                     
061525     END-IF                                                               
061526                                                                          
061527     INSPECT MID-IDDC65-IN REPLACING LEADING SPACE BY ZERO                
061528     IF MID-IDDC65-IN NOT = ALL '+'                                       
061529       IF MID-IDDC65-IN NOT NUMERIC                                       
061530         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC65-IN-ATTR                     
061531         MOVE NEJ TO INDATA-SW                                            
061532       ELSE                                                               
061533         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC65-IN-ATTR                   
061534       END-IF                                                             
061535     ELSE                                                                 
061536       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC65-IN-ATTR                     
061537     END-IF                                                               
061538                                                                          
061539     INSPECT MID-IDDC66-IN REPLACING LEADING SPACE BY ZERO                
061540     IF MID-IDDC66-IN NOT = ALL '+'                                       
061541       IF MID-IDDC66-IN NOT NUMERIC                                       
061542         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC66-IN-ATTR                     
061543         MOVE NEJ TO INDATA-SW                                            
061544       ELSE                                                               
061545         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC66-IN-ATTR                   
061546       END-IF                                                             
061547     ELSE                                                                 
061548       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC66-IN-ATTR                     
061549     END-IF                                                               
061550                                                                          
061551     INSPECT MID-IDDC67-IN REPLACING LEADING SPACE BY ZERO                
061552     IF MID-IDDC67-IN NOT = ALL '+'                                       
061553       IF MID-IDDC67-IN NOT NUMERIC                                       
061554         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC67-IN-ATTR                     
061555         MOVE NEJ TO INDATA-SW                                            
061556       ELSE                                                               
061557         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC67-IN-ATTR                   
061558       END-IF                                                             
061559     ELSE                                                                 
061560       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC67-IN-ATTR                     
061570     END-IF                                                               
061580                                                                          
061590     INSPECT MID-IDDC85-IN REPLACING LEADING SPACE BY ZERO                
061600     IF MID-IDDC85-IN NOT = ALL '+'                                       
061700       IF MID-IDDC85-IN NOT NUMERIC                                       
061800         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC85-IN-ATTR                     
061900         MOVE NEJ TO INDATA-SW                                            
062000       ELSE                                                               
062100         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC85-IN-ATTR                   
062200       END-IF                                                             
062300     ELSE                                                                 
062400       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC85-IN-ATTR                     
062500     END-IF                                                               
062600                                                                          
062700     INSPECT MID-IDDC86-IN REPLACING LEADING SPACE BY ZERO                
062800     IF MID-IDDC86-IN NOT = ALL '+'                                       
062900       IF MID-IDDC86-IN NOT NUMERIC                                       
063000         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC86-IN-ATTR                     
063100         MOVE NEJ TO INDATA-SW                                            
063200       ELSE                                                               
063300         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC86-IN-ATTR                   
063400       END-IF                                                             
063500     ELSE                                                                 
063600       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC86-IN-ATTR                     
063700     END-IF                                                               
065102                                                                          
065103     INSPECT MID-IDDC87-IN REPLACING LEADING SPACE BY ZERO                
065104     IF MID-IDDC87-IN NOT = ALL '+'                                       
065105       IF MID-IDDC87-IN NOT NUMERIC                                       
065106         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDC87-IN-ATTR                     
065107         MOVE NEJ TO INDATA-SW                                            
065108       ELSE                                                               
065109         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC87-IN-ATTR                   
065110       END-IF                                                             
065120     ELSE                                                                 
065130       MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDC87-IN-ATTR                     
065140     END-IF                                                               
065150                                                                          
065202     IF INDATA-FEL                                                        
065302       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
065402       CALL WMEDKONV USING MED-WMEDAREA                                   
065502       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
065602       PERFORM MFS-ROER-EJ-FAELT-UT                                       
065702       PERFORM MFS-ROER-EJ-FAELT-IN                                       
065802     ELSE                                                                 
065902       PERFORM IMS-GET-WDF1-LEV                                           
066002       IF SEGMENT-FINNS                                                   
066102         CONTINUE                                                         
066202       ELSE                                                               
066302         MOVE ERR-WRONG-SUPPL TO MED-IDMFSFEL                             
066402         CALL WMEDKONV USING MED-WMEDAREA                                 
066502         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
066602         PERFORM MFS-RENSA-FAELT-IN                                       
066702         PERFORM MFS-RENSA-FAELT-UT                                       
066802         MOVE NEJ TO INDATA-SW                                            
066902       END-IF                                                             
067002     END-IF                                                               
067102     .                                                                    
067202     EJECT                                                                
067302 H-UPPDATERA SECTION.                                                     
067402                                                                          
067802     PERFORM HA-UPPDATERA-DC51                                            
067803     PERFORM HA-UPPDATERA-DC52                                            
067804     PERFORM HA-UPPDATERA-DC53                                            
067902     PERFORM HB-UPPDATERA-DC61                                            
068002     PERFORM HC-UPPDATERA-DC62                                            
068003     PERFORM HD-UPPDATERA-DC63                                            
068004     PERFORM HE-UPPDATERA-DC64                                            
068005     PERFORM HF-UPPDATERA-DC65                                            
068006     PERFORM HG-UPPDATERA-DC66                                            
068007     PERFORM HH-UPPDATERA-DC67                                            
068008     PERFORM HI-UPPDATERA-DC85                                            
068009     PERFORM HI-UPPDATERA-DC86                                            
068010     PERFORM HI-UPPDATERA-DC87                                            
068402     .                                                                    
068502     EJECT                                                                
078502 HA-UPPDATERA-DC51  SECTION.                                              
078602                                                                          
078702     MOVE WC-NDC-CA     TO W-IDDC                                         
078802     PERFORM IMS-GET-WDF1-NDC                                             
078902                                                                          
079002     IF SEGMENT-FINNS                                                     
079102        IF MID-IDDC51-IN NOT = ALL '+'                                    
079202           MOVE MID-IDDC51-IN TO WDF1-NDC-KVDAGAR-TBT                     
079302        ELSE                                                              
079402           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC51-IN                        
079502        END-IF                                                            
079602        PERFORM IMS-REPL-WDF1-NDC                                         
079702     ELSE                                                                 
079803        MOVE WC-NDC-CA    TO WDF1-NDC-IDDC                                
079804        PERFORM S01-INITIATE-WDF116                                       
080002        IF MID-IDDC51-IN NOT = ALL '+'                                    
080102           MOVE MID-IDDC51-IN TO WDF1-NDC-KVDAGAR-TBT                     
080402        END-IF                                                            
080502        PERFORM IMS-ISRT-WDF1-NDC                                         
080602     END-IF                                                               
080702     .                                                                    
080802     EJECT                                                                
080803 HA-UPPDATERA-DC52  SECTION.                                              
080804                                                                          
080805     MOVE WC-NDC-BR     TO W-IDDC                                         
080806     PERFORM IMS-GET-WDF1-NDC                                             
080807                                                                          
080808     IF SEGMENT-FINNS                                                     
080809        IF MID-IDDC52-IN NOT = ALL '+'                                    
080810           MOVE MID-IDDC52-IN TO WDF1-NDC-KVDAGAR-TBT                     
080820        ELSE                                                              
080830           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC52-IN                        
080840        END-IF                                                            
080850        PERFORM IMS-REPL-WDF1-NDC                                         
080860     ELSE                                                                 
080870        MOVE WC-NDC-BR    TO WDF1-NDC-IDDC                                
080880        PERFORM S01-INITIATE-WDF116                                       
080890        IF MID-IDDC52-IN NOT = ALL '+'                                    
080900           MOVE MID-IDDC52-IN TO WDF1-NDC-KVDAGAR-TBT                     
080901        END-IF                                                            
080902        PERFORM IMS-ISRT-WDF1-NDC                                         
080903     END-IF                                                               
080904     .                                                                    
080905     EJECT                                                                
080906 HA-UPPDATERA-DC53  SECTION.                                              
080907                                                                          
080908     MOVE WC-NDC-MX     TO W-IDDC                                         
080909     PERFORM IMS-GET-WDF1-NDC                                             
080910                                                                          
080911     IF SEGMENT-FINNS                                                     
080912        IF MID-IDDC53-IN NOT = ALL '+'                                    
080913           MOVE MID-IDDC53-IN TO WDF1-NDC-KVDAGAR-TBT                     
080914        ELSE                                                              
080915           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC53-IN                        
080916        END-IF                                                            
080917        PERFORM IMS-REPL-WDF1-NDC                                         
080918     ELSE                                                                 
080919        MOVE WC-NDC-MX    TO WDF1-NDC-IDDC                                
080920        PERFORM S01-INITIATE-WDF116                                       
080921        IF MID-IDDC53-IN NOT = ALL '+'                                    
080922           MOVE MID-IDDC53-IN TO WDF1-NDC-KVDAGAR-TBT                     
080923        END-IF                                                            
080924        PERFORM IMS-ISRT-WDF1-NDC                                         
080925     END-IF                                                               
080926     .                                                                    
080927     EJECT                                                                
080930 HB-UPPDATERA-DC61  SECTION.                                              
081002                                                                          
081102     MOVE WC-NDC-JP     TO W-IDDC                                         
081202     PERFORM IMS-GET-WDF1-NDC                                             
081302                                                                          
081402     IF SEGMENT-FINNS                                                     
081502        IF MID-IDDC61-IN NOT = ALL '+'                                    
081602           MOVE MID-IDDC61-IN TO WDF1-NDC-KVDAGAR-TBT                     
081702        ELSE                                                              
081802           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC61-IN                        
081902        END-IF                                                            
082002        PERFORM IMS-REPL-WDF1-NDC                                         
082102     ELSE                                                                 
082203        MOVE WC-NDC-JP    TO WDF1-NDC-IDDC                                
082204        PERFORM S01-INITIATE-WDF116                                       
082402        IF MID-IDDC61-IN NOT = ALL '+'                                    
082502           MOVE MID-IDDC61-IN TO WDF1-NDC-KVDAGAR-TBT                     
082802        END-IF                                                            
082902        PERFORM IMS-ISRT-WDF1-NDC                                         
083002     END-IF                                                               
083102     .                                                                    
083202     EJECT                                                                
083302 HC-UPPDATERA-DC62  SECTION.                                              
083402                                                                          
083502     MOVE WC-NDC-AU     TO W-IDDC                                         
083602     PERFORM IMS-GET-WDF1-NDC                                             
083702                                                                          
083802     IF SEGMENT-FINNS                                                     
083902        IF MID-IDDC62-IN NOT = ALL '+'                                    
084002           MOVE MID-IDDC62-IN TO WDF1-NDC-KVDAGAR-TBT                     
084102        ELSE                                                              
084202           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC62-IN                        
084302        END-IF                                                            
084402        PERFORM IMS-REPL-WDF1-NDC                                         
084502     ELSE                                                                 
084603        MOVE WC-NDC-AU    TO WDF1-NDC-IDDC                                
084604        PERFORM S01-INITIATE-WDF116                                       
084802        IF MID-IDDC62-IN NOT = ALL '+'                                    
084902           MOVE MID-IDDC62-IN TO WDF1-NDC-KVDAGAR-TBT                     
085202        END-IF                                                            
085302        PERFORM IMS-ISRT-WDF1-NDC                                         
085402     END-IF                                                               
085502     .                                                                    
085602     EJECT                                                                
085603 HD-UPPDATERA-DC63  SECTION.                                              
085604                                                                          
085605     MOVE WC-NDC-TH     TO W-IDDC                                         
085606     PERFORM IMS-GET-WDF1-NDC                                             
085607                                                                          
085608     IF SEGMENT-FINNS                                                     
085609        IF MID-IDDC63-IN NOT = ALL '+'                                    
085610           MOVE MID-IDDC63-IN TO WDF1-NDC-KVDAGAR-TBT                     
085611        ELSE                                                              
085612           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC63-IN                        
085613        END-IF                                                            
085614        PERFORM IMS-REPL-WDF1-NDC                                         
085615     ELSE                                                                 
085616        MOVE WC-NDC-TH    TO WDF1-NDC-IDDC                                
085617        PERFORM S01-INITIATE-WDF116                                       
085618        IF MID-IDDC63-IN NOT = ALL '+'                                    
085619           MOVE MID-IDDC63-IN TO WDF1-NDC-KVDAGAR-TBT                     
085620        END-IF                                                            
085621        PERFORM IMS-ISRT-WDF1-NDC                                         
085622     END-IF                                                               
085623     .                                                                    
085624     EJECT                                                                
085625 HE-UPPDATERA-DC64  SECTION.                                              
085626                                                                          
085627     MOVE WC-NDC-TW     TO W-IDDC                                         
085628     PERFORM IMS-GET-WDF1-NDC                                             
085629                                                                          
085630     IF SEGMENT-FINNS                                                     
085631        IF MID-IDDC64-IN NOT = ALL '+'                                    
085632           MOVE MID-IDDC64-IN TO WDF1-NDC-KVDAGAR-TBT                     
085633        ELSE                                                              
085634           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC64-IN                        
085635        END-IF                                                            
085636        PERFORM IMS-REPL-WDF1-NDC                                         
085637     ELSE                                                                 
085638        MOVE WC-NDC-TW    TO WDF1-NDC-IDDC                                
085639        PERFORM S01-INITIATE-WDF116                                       
085640        IF MID-IDDC64-IN NOT = ALL '+'                                    
085641           MOVE MID-IDDC64-IN TO WDF1-NDC-KVDAGAR-TBT                     
085642        END-IF                                                            
085643        PERFORM IMS-ISRT-WDF1-NDC                                         
085644     END-IF                                                               
085645     .                                                                    
085646     EJECT                                                                
085647 HF-UPPDATERA-DC65  SECTION.                                              
085648                                                                          
085649     MOVE WC-NDC-KR     TO W-IDDC                                         
085650     PERFORM IMS-GET-WDF1-NDC                                             
085651                                                                          
085652     IF SEGMENT-FINNS                                                     
085653        IF MID-IDDC65-IN NOT = ALL '+'                                    
085654           MOVE MID-IDDC65-IN TO WDF1-NDC-KVDAGAR-TBT                     
085655        ELSE                                                              
085656           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC65-IN                        
085657        END-IF                                                            
085658        PERFORM IMS-REPL-WDF1-NDC                                         
085659     ELSE                                                                 
085660        MOVE WC-NDC-KR    TO WDF1-NDC-IDDC                                
085661        PERFORM S01-INITIATE-WDF116                                       
085662        IF MID-IDDC65-IN NOT = ALL '+'                                    
085663           MOVE MID-IDDC65-IN TO WDF1-NDC-KVDAGAR-TBT                     
085664        END-IF                                                            
085665        PERFORM IMS-ISRT-WDF1-NDC                                         
085666     END-IF                                                               
085667     .                                                                    
085668     EJECT                                                                
085669 HG-UPPDATERA-DC66  SECTION.                                              
085670                                                                          
085671     MOVE WC-NDC-MY     TO W-IDDC                                         
085672     PERFORM IMS-GET-WDF1-NDC                                             
085673                                                                          
085674     IF SEGMENT-FINNS                                                     
085675        IF MID-IDDC66-IN NOT = ALL '+'                                    
085676           MOVE MID-IDDC66-IN TO WDF1-NDC-KVDAGAR-TBT                     
085677        ELSE                                                              
085678           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC66-IN                        
085679        END-IF                                                            
085680        PERFORM IMS-REPL-WDF1-NDC                                         
085681     ELSE                                                                 
085682        MOVE WC-NDC-MY    TO WDF1-NDC-IDDC                                
085683        PERFORM S01-INITIATE-WDF116                                       
085684        IF MID-IDDC66-IN NOT = ALL '+'                                    
085685           MOVE MID-IDDC66-IN TO WDF1-NDC-KVDAGAR-TBT                     
085686        END-IF                                                            
085687        PERFORM IMS-ISRT-WDF1-NDC                                         
085688     END-IF                                                               
085689     .                                                                    
085690     EJECT                                                                
085691 HH-UPPDATERA-DC67  SECTION.                                              
085692                                                                          
085693     MOVE WC-NDC-IN     TO W-IDDC                                         
085694     PERFORM IMS-GET-WDF1-NDC                                             
085695                                                                          
085696     IF SEGMENT-FINNS                                                     
085697        IF MID-IDDC67-IN NOT = ALL '+'                                    
085698           MOVE MID-IDDC67-IN TO WDF1-NDC-KVDAGAR-TBT                     
085699        ELSE                                                              
085700           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC67-IN                        
085701        END-IF                                                            
085702        PERFORM IMS-REPL-WDF1-NDC                                         
085703     ELSE                                                                 
085704        MOVE WC-NDC-IN    TO WDF1-NDC-IDDC                                
085705        PERFORM S01-INITIATE-WDF116                                       
085706        IF MID-IDDC67-IN NOT = ALL '+'                                    
085707           MOVE MID-IDDC67-IN TO WDF1-NDC-KVDAGAR-TBT                     
085708        END-IF                                                            
085709        PERFORM IMS-ISRT-WDF1-NDC                                         
085710     END-IF                                                               
085711     .                                                                    
085720     EJECT                                                                
085730 HI-UPPDATERA-DC85  SECTION.                                              
085740                                                                          
085750     MOVE WC-NDC-ZA     TO W-IDDC                                         
085760     PERFORM IMS-GET-WDF1-NDC                                             
085770                                                                          
085780     IF SEGMENT-FINNS                                                     
085790        IF MID-IDDC85-IN NOT = ALL '+'                                    
085800           MOVE MID-IDDC85-IN TO WDF1-NDC-KVDAGAR-TBT                     
085900        ELSE                                                              
086000           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC85-IN                        
086100        END-IF                                                            
086200        PERFORM IMS-REPL-WDF1-NDC                                         
086300     ELSE                                                                 
086400        MOVE WC-NDC-ZA    TO WDF1-NDC-IDDC                                
086500        PERFORM S01-INITIATE-WDF116                                       
086600        IF MID-IDDC85-IN NOT = ALL '+'                                    
086700           MOVE MID-IDDC85-IN TO WDF1-NDC-KVDAGAR-TBT                     
086800        END-IF                                                            
086900        PERFORM IMS-ISRT-WDF1-NDC                                         
087000     END-IF                                                               
087100     .                                                                    
087200     EJECT                                                                
087300 HI-UPPDATERA-DC86  SECTION.                                              
087400                                                                          
087500     MOVE WC-NDC-TR     TO W-IDDC                                         
087600     PERFORM IMS-GET-WDF1-NDC                                             
087700                                                                          
087800     IF SEGMENT-FINNS                                                     
087900        IF MID-IDDC86-IN NOT = ALL '+'                                    
088000           MOVE MID-IDDC86-IN TO WDF1-NDC-KVDAGAR-TBT                     
088100        ELSE                                                              
088200           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC86-IN                        
088300        END-IF                                                            
088400        PERFORM IMS-REPL-WDF1-NDC                                         
088500     ELSE                                                                 
088600        MOVE WC-NDC-TR    TO WDF1-NDC-IDDC                                
088700        PERFORM S01-INITIATE-WDF116                                       
088800        IF MID-IDDC86-IN NOT = ALL '+'                                    
088900           MOVE MID-IDDC86-IN TO WDF1-NDC-KVDAGAR-TBT                     
089000        END-IF                                                            
089100        PERFORM IMS-ISRT-WDF1-NDC                                         
089200     END-IF                                                               
089300     .                                                                    
089400     EJECT                                                                
094102                                                                          
094103 HI-UPPDATERA-DC87  SECTION.                                              
094104                                                                          
094105     MOVE WC-NDC-AE     TO W-IDDC                                         
094106     PERFORM IMS-GET-WDF1-NDC                                             
094107                                                                          
094108     IF SEGMENT-FINNS                                                     
094109        IF MID-IDDC87-IN NOT = ALL '+'                                    
094110           MOVE MID-IDDC87-IN TO WDF1-NDC-KVDAGAR-TBT                     
094120        ELSE                                                              
094130           MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC87-IN                        
094140        END-IF                                                            
094150        PERFORM IMS-REPL-WDF1-NDC                                         
094160     ELSE                                                                 
094170        MOVE WC-NDC-AE    TO WDF1-NDC-IDDC                                
094180        PERFORM S01-INITIATE-WDF116                                       
094190        IF MID-IDDC87-IN NOT = ALL '+'                                    
094200           MOVE MID-IDDC87-IN TO WDF1-NDC-KVDAGAR-TBT                     
094201        END-IF                                                            
094202        PERFORM IMS-ISRT-WDF1-NDC                                         
094203     END-IF                                                               
094204     .                                                                    
094205     EJECT                                                                
094206                                                                          
094207                                                                          
094302     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
094402     CALL WMEDKONV        USING MED-WMEDAREA                              
094502     MOVE MED-MFSINF      TO MOD-TEMFSINF                                 
094602     PERFORM MFS-FORM-ATTR                                                
094702     PERFORM MFS-RENSA-FAELT-IN                                           
094802* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
094902     .                                                                    
095002     EJECT                                                                
095007 S01-INITIATE-WDF116 SECTION.                                             
095008                                                                          
095009     MOVE +0                  TO WDF1-NDC-KVDAGAR-AVIAVV                  
095010                                 WDF1-NDC-KVDAGAR-INLAVV                  
095011                                 WDF1-NDC-KVDAGAR-TBT                     
095012                                 WDF1-NDC-KVDAGAR-TT                      
095013                                 WDF1-NDC-KVVECKOR-LVAR                   
095014                                                                          
095015     MOVE +1                  TO TAB-IX1                                  
095020     PERFORM UNTIL TAB-IX1 > TAB-IX1-MAX                                  
095030        MOVE +0               TO WDF1-NDC-TILEVDAG (TAB-IX1)              
095040        ADD +1                TO TAB-IX1                                  
095050     END-PERFORM                                                          
095060                                                                          
095070     MOVE +1                  TO TAB-IX2                                  
095080     PERFORM UNTIL TAB-IX2 > TAB-IX2-MAX                                  
095090        MOVE +0               TO WDF1-NDC-IDANSK-PG (TAB-IX2)             
095100        ADD +1                TO TAB-IX2                                  
095101     END-PERFORM                                                          
095102     .                                                                    
095103     EJECT                                                                
095104 MFS-RENSA-FAELT-UT SECTION.                                              
095202                                                                          
095302*    --- ALLA UTDATA-FÄLT                                                 
095402     MOVE MFS-RENSA-FAELT    TO                                           
095502                                MOD-BELEV                                 
095602                                MOD-IDDC51-UT                             
095603                                MOD-IDDC52-UT                             
095604                                MOD-IDDC53-UT                             
095702                                MOD-ADLEV-RAD1                            
095802                                MOD-ADLEV-RAD2                            
095902                                MOD-IDDC61-UT                             
096002                                MOD-ADLEV-ORT                             
096102                                MOD-ADLEVLND                              
096202                                MOD-IDDC62-UT                             
096302                                MOD-IDDC63-UT                             
096402                                MOD-IDDC64-UT                             
096502                                MOD-IDDC65-UT                             
096503                                MOD-IDDC66-UT                             
096602                                MOD-IDDC67-UT                             
096603                                MOD-IDDC85-UT                             
096604                                MOD-IDDC86-UT                             
096605                                MOD-IDDC87-UT                             
096902                                MOD-TEFRAKT                               
097002                                MOD-TEFRAVIL                              
097102                                MOD-TETERMS                               
097202     .                                                                    
097302     SKIP3                                                                
097402 MFS-RENSA-FAELT-IN SECTION.                                              
097502                                                                          
097602*    --- ALLA INDATA-FÄLT                                                 
097702     MOVE MFS-RENSA-FAELT    TO                                           
097802                                MOD-IDDC51-IN                             
097803                                MOD-IDDC52-IN                             
097804                                MOD-IDDC53-IN                             
097902                                MOD-IDDC61-IN                             
098002                                MOD-IDDC62-IN                             
098102                                MOD-IDDC63-IN                             
098202                                MOD-IDDC64-IN                             
098302                                MOD-IDDC65-IN                             
098303                                MOD-IDDC66-IN                             
098402                                MOD-IDDC67-IN                             
098403                                MOD-IDDC85-IN                             
098404                                MOD-IDDC86-IN                             
098405                                MOD-IDDC87-IN                             
098702                                MOD-TEFRAKT                               
098802                                MOD-TEFRAVIL                              
098902                                MOD-TETERMS                               
099002     .                                                                    
099102     EJECT                                                                
099202 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
099302                                                                          
099402*    --- ALLA UTDATA-FÄLT                                                 
099502     MOVE MFS-ROER-EJ-FAELT  TO                                           
099602                                MOD-BELEV                                 
099702                                MOD-IDDC51-UT                             
099703                                MOD-IDDC52-UT                             
099704                                MOD-IDDC53-UT                             
099802                                MOD-ADLEV-RAD1                            
099902                                MOD-ADLEV-RAD2                            
100002                                MOD-IDDC61-UT                             
100102                                MOD-ADLEV-ORT                             
100202                                MOD-ADLEVLND                              
100302                                MOD-IDDC62-UT                             
100402                                MOD-IDDC63-UT                             
100502                                MOD-IDDC64-UT                             
100602                                MOD-IDDC65-UT                             
100603                                MOD-IDDC66-UT                             
100702                                MOD-IDDC67-UT                             
100703                                MOD-IDDC85-UT                             
100704                                MOD-IDDC86-UT                             
100705                                MOD-IDDC87-UT                             
101002                                MOD-TEFRAKT                               
101102                                MOD-TEFRAVIL                              
101202                                MOD-TETERMS                               
101302     .                                                                    
101402     SKIP3                                                                
101502 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
101602                                                                          
101702*    --- ALLA INDATA-FÄLT                                                 
101802     MOVE MFS-ROER-EJ-FAELT  TO                                           
101902                                MOD-IDDC51-IN                             
101903                                MOD-IDDC52-IN                             
101904                                MOD-IDDC53-IN                             
102002                                MOD-IDDC61-IN                             
102102                                MOD-IDDC62-IN                             
102202                                MOD-IDDC63-IN                             
102302                                MOD-IDDC64-IN                             
102402                                MOD-IDDC65-IN                             
102403                                MOD-IDDC66-IN                             
102502                                MOD-IDDC67-IN                             
102503                                MOD-IDDC85-IN                             
102504                                MOD-IDDC86-IN                             
102505                                MOD-IDDC87-IN                             
102802                                MOD-TEFRAKT                               
102902                                MOD-TEFRAVIL                              
103002                                MOD-TETERMS                               
103102     .                                                                    
103202     EJECT                                                                
103302 MFS-STAENG-FAELT-IN  SECTION.                                            
103402                                                                          
103502*    --- ALLA INDATA-FÄLT                                                 
103602     MOVE MFS-STAENG-FAELT  TO                                            
103702                                MOD-IDDC51-IN-ATTR                        
103703                                MOD-IDDC52-IN-ATTR                        
103704                                MOD-IDDC53-IN-ATTR                        
103802                                MOD-IDDC61-IN-ATTR                        
103902                                MOD-IDDC62-IN-ATTR                        
104002                                MOD-IDDC63-IN-ATTR                        
104102                                MOD-IDDC64-IN-ATTR                        
104202                                MOD-IDDC65-IN-ATTR                        
104203                                MOD-IDDC66-IN-ATTR                        
104302                                MOD-IDDC67-IN-ATTR                        
104303                                MOD-IDDC85-IN-ATTR                        
104304                                MOD-IDDC86-IN-ATTR                        
104305                                MOD-IDDC87-IN-ATTR                        
104602                                MOD-TEFRAKT-ATTR                          
104702                                MOD-TEFRAVIL-ATTR                         
104802                                MOD-TETERMS-ATTR                          
104902     .                                                                    
105002     EJECT                                                                
105102 MFS-FORM-ATTR SECTION.                                                   
105202                                                                          
105302*    --- ALLA INDATA-FÄLT                                                 
105402     MOVE MFS-FORMATETS-ATTR TO                                           
105502                                MOD-IDDC51-IN-ATTR                        
105503                                MOD-IDDC52-IN-ATTR                        
105504                                MOD-IDDC53-IN-ATTR                        
105602                                MOD-IDDC61-IN-ATTR                        
105702                                MOD-IDDC62-IN-ATTR                        
105802                                MOD-IDDC63-IN-ATTR                        
105902                                MOD-IDDC64-IN-ATTR                        
106002                                MOD-IDDC65-IN-ATTR                        
106003                                MOD-IDDC66-IN-ATTR                        
106102                                MOD-IDDC67-IN-ATTR                        
106103                                MOD-IDDC85-IN-ATTR                        
106104                                MOD-IDDC86-IN-ATTR                        
106105                                MOD-IDDC87-IN-ATTR                        
106402                                MOD-TEFRAKT-ATTR                          
106502                                MOD-TEFRAVIL-ATTR                         
106602                                MOD-TETERMS-ATTR                          
106702     .                                                                    
106802     SKIP2                                                                
106902* --- IMS SEKTIONER ---                                                   
107002     SKIP3                                                                
107102 IMS-GET-MSG SECTION.                                                     
107202                                                                          
107302     MOVE '  QC' TO GODK-STATUSKODER                                      
107402     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
107502     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107602     PERFORM IMS-STATUSKONTROLL                                           
107702     .                                                                    
107802     SKIP3                                                                
107902 IMS-INSERT-MSG SECTION.                                                  
108002                                                                          
108102     IF MSGI-IDLAND-SPR = 'GB'                                            
108202       MOVE 'N' TO MFS-KDHUVOMR                                           
108302     END-IF                                                               
108402     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
108502     MOVE SPACE TO GODK-STATUSKODER                                       
108602     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
108702     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
108802     PERFORM IMS-STATUSKONTROLL                                           
108902     .                                                                    
109002     EJECT                                                                
109102 IMS-GET-WDF1-LEV SECTION.                                                
109202                                                                          
109302     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
109402          DELIMITED BY SIZE INTO SSA1                                     
109502     MOVE '  GE' TO GODK-STATUSKODER                                      
109602     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
109702     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
109802     PERFORM IMS-STATUSKONTROLL                                           
109902     .                                                                    
110002     EJECT                                                                
110102 IMS-GET-WDF1-ADR SECTION.                                                
110202                                                                          
110302     MOVE 'WDF106   ' TO SSA1                                             
110402     MOVE '  GE' TO GODK-STATUSKODER                                      
110502     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF106 SSA1                   
110602     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
110702     PERFORM IMS-STATUSKONTROLL                                           
110802     .                                                                    
110902     EJECT                                                                
111002 IMS-GET-WDF1-NDC SECTION.                                                
111102                                                                          
111202     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
111302          DELIMITED BY SIZE INTO SSA1                                     
111402     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
111502          DELIMITED BY SIZE INTO SSA2                                     
111602     MOVE '  GE' TO GODK-STATUSKODER                                      
111702     CALL CBLTDLI USING GHU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2              
111802     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
111902     PERFORM IMS-STATUSKONTROLL                                           
112002     .                                                                    
112102     SKIP3                                                                
112202 IMS-GET-WDF1-TXT SECTION.                                                
112302                                                                          
112404     MOVE 'WDF121    '      TO SSA1                                       
112502     MOVE '  GE'            TO GODK-STATUSKODER                           
112602     CALL CBLTDLI USING GHNP WDF1-PCB DLI-IO-WDF121 SSA1                  
112702     MOVE WDF1-STATUS-CODE  TO STATUS-WS                                  
112802     PERFORM IMS-STATUSKONTROLL                                           
112902     .                                                                    
113002     SKIP3                                                                
113102 IMS-ISRT-WDF1-NDC SECTION.                                               
113202                                                                          
113302     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
113402          DELIMITED BY SIZE INTO SSA1                                     
113502     MOVE 'WDF116   ' TO SSA2                                             
113602     MOVE '  II' TO GODK-STATUSKODER                                      
113702     CALL CBLTDLI USING ISRT WDF1-PCB DLI-IO-WDF116 SSA1 SSA2             
113802     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
113902     PERFORM IMS-STATUSKONTROLL                                           
114002     .                                                                    
114102     SKIP3                                                                
114202 IMS-ISRT-WDF1-TXT SECTION.                                               
114302                                                                          
114402     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
114502          DELIMITED BY SIZE INTO SSA1                                     
114602     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
114702          DELIMITED BY SIZE INTO SSA2                                     
114802     MOVE 'WDF121   ' TO SSA3                                             
114902     MOVE '  II' TO GODK-STATUSKODER                                      
115002     CALL CBLTDLI USING ISRT WDF1-PCB DLI-IO-WDF121                       
115102                             SSA1 SSA2 SSA3                               
115202     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
115302     PERFORM IMS-STATUSKONTROLL                                           
115402     .                                                                    
115502     SKIP3                                                                
115602 IMS-REPL-WDF1-NDC SECTION.                                               
115702                                                                          
115802     MOVE '  ' TO GODK-STATUSKODER                                        
115902     CALL CBLTDLI USING REPL WDF1-PCB DLI-IO-WDF116                       
116002     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
116102     PERFORM IMS-STATUSKONTROLL                                           
116202     .                                                                    
116302     EJECT                                                                
116402 IMS-REPL-WDF1-TXT SECTION.                                               
116502                                                                          
116602     MOVE '  ' TO GODK-STATUSKODER                                        
116702     CALL CBLTDLI USING REPL WDF1-PCB DLI-IO-WDF121                       
116802     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
116902     PERFORM IMS-STATUSKONTROLL                                           
117002     .                                                                    
117102     EJECT                                                                
117202 IMS-STATUSKONTROLL SECTION.                                              
117302                                                                          
117402     SET STATUS-IX TO 1                                                   
117502     SEARCH GODK-STATUS                                                   
117602       AT END                                                             
117702         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
117802         DELIMITED BY SIZE INTO FELTEXT                                   
117902         CALL FELLOG                                                      
118002       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
118102         CONTINUE                                                         
119002     END-SEARCH                                                           
120002     .                                                                    
