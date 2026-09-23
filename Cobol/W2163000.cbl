000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2163000.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   98/10/29.                                                
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER FIL W216PP MED URVAL FRÅN BILD 6322                        
000900*        STARTAR TRANS W4T251 W4T252                                      
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WL6321 (WDR5)                              
001200*                                                                         
001300*  2011-10-27   E'TRACKER 10143271 CHINA  WAREHOUSE PROJECT-1             
001400*  2012-07-10   E'TRACKER 8200058  MANAGEMENT SCRAPPING FOLLOW UP         
001500*                                                                         
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100     SKIP2                                                                
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- URVAL FRÅN 6322                                            
002500     SELECT W216PP                     ASSIGN TO W21630D1.                
002600     SKIP2                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP2                                                                
003100 FD  W216PP                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400 01  PARM             PIC X(80).                                          
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W2163000'.            
003900 77  ORAD-IX                     PIC S9(3)   VALUE ZERO.                  
004000 77  ORAD-IX-MAX                 PIC S9(3)   VALUE +5.                    
004100 77  PARM-IX                     PIC S9(3)   VALUE ZERO.                  
004200 77  PARM-IX-MAX                 PIC S9(3)   VALUE +12.                   
004300 77  TAB-IX                      PIC S9(3)   VALUE ZERO.                  
004400 77  TAB-IX-MAX                  PIC S9(3)   VALUE +12.                   
004500 77  TAB2-IX                     PIC S9(3)   VALUE ZERO.                  
004600 77  TAB2-IX-MAX                 PIC S9(3)   VALUE +12.                   
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  WS-OHUV-SKAPAD              PIC X       VALUE 'N'.                   
005000 77  WS-TAB-UPD                  PIC X       VALUE 'N'.                   
005100 77  WS-TABESC-UPD               PIC X       VALUE 'N'.                   
005200 77  WS-ENSTAKA-SKROTORDER       PIC X       VALUE 'N'.                   
005300 77  WS-KVSKROT-BEORD            PIC 9(6)    VALUE ZERO.                  
005400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
005600 77  SPAR-IDDISTR                PIC S9(5) COMP-3.                        
005700 77  SPAR-IDKUNDNR               PIC S9(7) COMP-3.                        
005800 77  SPAR-KDFRAKT                PIC S9(3) COMP-3.                        
005900 77  SPAR-IDARTNR                PIC S9(9) COMP-3.                        
006000 77  SPAR-DASKROT9-BEORD         PIC 9(8).                                
006100 77  SPAR2-IDDC                   PIC X(2)    VALUE SPACE.                
006200 77  SPAR2-IDDISTR                PIC S9(5) COMP-3.                       
006300 77  SPAR2-IDKUNDNR               PIC S9(7) COMP-3.                       
006400 77  SPAR2-KDFRAKT                PIC S9(3) COMP-3.                       
006500 77  SPAR2-IDARTNR                PIC S9(9) COMP-3.                       
006600 77  SPAR2-DASKROT9-BEORD         PIC 9(8).                               
006700 77  WS-IDKONTO-DISPLAY          PIC 9(10)  VALUE ZERO.                   
006800 77  WS-KDFRAKT-DISPLAY          PIC 9(2)   VALUE ZERO.                   
006900 77  WS-KDORDKL-DISPLAY          PIC 9      VALUE ZERO.                   
007000                                                                          
007100 01  WS-IDORDNR-NUM                          PIC 9(7).                    
007200 01  WS-IDORDNR REDEFINES WS-IDORDNR-NUM     PIC X(7).                    
007300 01  WS-IDDISTR-NUM                          PIC 9(4).                    
007400 01  WS-IDDISTR REDEFINES WS-IDDISTR-NUM     PIC X(4).                    
007500 01  WS-IDKUNDNR-NUM                         PIC 9(6).                    
007600 01  WS-IDKUNDNR REDEFINES WS-IDKUNDNR-NUM   PIC X(6).                    
007700                                                                          
007800 01  KONTROLL-SIFFRA.                                                     
007900   03  REK-IDARTNR               PIC 9(9)    VALUE 0.                     
008000   03  REK-LNGD                  PIC 9(1)    VALUE 9.                     
008100   03  REK-REKSIFFR              PIC 9(1)    VALUE 0.                     
008200                                                                          
008300 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
008400     88  END-OF-W216PP                       VALUE 'J'.                   
008500                                                                          
008600 01  FELTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900                                                                          
009000 01  TABENTRY-PARM.                                                       
009100     03  STEGLAANGD              PIC S9(9) COMP.                          
009200     03  ANTAL                   PIC S9(9) COMP.                          
009300     03  NYCKELLAANGD            PIC S9(9) COMP.                          
009400     EJECT                                                                
009500 01  SORT-TABELL.                                                         
009600     03  TAB-RAD OCCURS 12.                                               
009700        05  TAB-SORT-BEGREPP.                                             
009800            07  TAB-IDDC        PIC X(2).                                 
009900            07  TAB-IDDISTR     PIC S9(5) COMP-3.                         
010000            07  TAB-IDKUNDNR    PIC S9(7) COMP-3.                         
010100            07  TAB-KDFRAKT     PIC S9(3) COMP-3.                         
010200        05  TAB-IDARTNR         PIC S9(9) COMP-3.                         
010300        05  TAB-DASKROT9-BEORD  PIC 9(8).                                 
010400                                                                          
010500       EJECT                                                              
010600 01  SORT-TABELL-ESC.                                                     
010700     03  TAB2-RAD OCCURS 12.                                              
010800        05  TAB2-SORT-BEGREPP.                                            
010900            07  TAB2-IDDC        PIC X(2).                                
011000            07  TAB2-IDDISTR     PIC S9(5) COMP-3.                        
011100            07  TAB2-IDKUNDNR    PIC S9(7) COMP-3.                        
011200            07  TAB2-KDFRAKT     PIC S9(3) COMP-3.                        
011300        05  TAB2-IDARTNR         PIC S9(9) COMP-3.                        
011400        05  TAB2-DASKROT9-BEORD  PIC 9(8).                                
011500        05  TAB2-IDUSER          PIC X(8).                                
011600                                                                          
011700       EJECT                                                              
011800                                                                          
011900*01 -COPY WWIDFTG                                                         
012000     EJECT                                                                
012100                                                                          
012200*01 -COPY WWDC99                                                          
012300     EJECT                                                                
012400                                                                          
012500 01  FILLER              PIC X(16) VALUE 'TEST-IDDISTRIKT '.              
012600                                                                          
012700 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
012800*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
012900                                                                          
013000     EJECT                                                                
013100 01  FILLER              PIC X(16)  VALUE 'TEST-IDKUNDNR'.                
013200*   -COPY WWKUND17                                                        
013300     EJECT                                                                
013400                                                                          
013500 01  DYNAMISKA-SUBPROGRAM.                                                
013600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
014000     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
014100     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
014200     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
014300     EJECT                                                                
014400 01  IN-AREA-START             PIC X(24)   VALUE                          
014500                                 'IN-AREA-START '.                        
014600 01  IN-AREA1.                                                            
014700     03 IN-FLKLAR                PIC X(1).                                
014800     03 IN-KDARBTYP              PIC X(8).                                
014900 01  PARM-AREA-START             PIC X(24)   VALUE                        
015000                                 'PARM-AREA-START '.                      
015100 01  PARM-AREA                   PIC X(500).                              
015200 01  FILLER REDEFINES PARM-AREA.                                          
015300     03 PARM-FLKLAR                PIC X(1).                              
015400     03 PARM-KDARBTYP              PIC X(8).                              
015500     03 PARM-TABELL.                                                      
015600        05 PARM-TAB-RAD OCCURS 12.                                        
015700           07 PARM-IDDC            PIC X(2).                              
015800           07 PARM-IDARTNR         PIC 9(9).                              
015900           07 PARM-DASKROT9-BEORD  PIC 9(8).                              
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)   VALUE 'W411ORDN'.            
016200*    --- PARAMETRAR TILL SUBPROGRAM W411ORDN                              
016300*01 -COPY W411ORDN                                                        
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016600*    --- PARAMETRAR TILL POSTSUM                                          
016700*01  -COPY W0005 -PRE  POSTSUM-                                           
016800     EJECT                                                                
016900*                                                                         
017000 01  NYCKLAR-TILL-DLI.                                                    
017100     03  W-IDARTNR-X.                                                     
017200         05  W-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.                
017300     03  W-IDDC-X.                                                        
017400         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
017500     03  W-KDARBTYP-X.                                                    
017600         05  W-KDARBTYP      PIC X(8)   VALUE SPACE.                      
017700     03  W-DASKROT9-X.                                                    
017800         05  W-DASKROT9      PIC 9(8)   VALUE ZERO.                       
017900     03  W-WDGXKEY-6321.                                                  
018000         05  W-6321-IDHTYP    PIC X(4)   VALUE '6321'.                    
018100         05  W-6321-KDARBTYP  PIC X(8)   VALUE SPACE.                     
018200         05  W-6321-LOWVALUE  PIC X(18)  VALUE LOW-VALUE.                 
018300     03  W-KY6324-MIN-X.                                                  
018400         05  W-6324-IDARTNR-MIN  PIC S9(9) VALUE ZERO COMP-3.             
018500         05  W-6324-IDDC-MIN     PIC X(2) VALUE SPACE.                    
018600         05  W-6324-KDSTASKR-MIN PIC S9   VALUE 0 COMP-3.                 
018700     03  W-KY6324-MAX-X.                                                  
018800         05  W-6324-IDARTNR-MAX  PIC S9(9) VALUE ZERO COMP-3.             
018900         05  W-6324-IDDC-MAX     PIC X(2) VALUE SPACE.                    
019000         05  W-6324-KDSTASKR-MAX PIC S9   VALUE 9 COMP-3.                 
019100     03  W-IDDC-B6-X.                                                     
019200         05 W-IDDC-B6                  PIC X(2).                          
019300     EJECT                                                                
019400*    --- STATUS-KOD FRÅN IMS                                              
019500 01  STATUS-WS                   PIC XX.                                  
019600     88  SEGMENT-FINNS                       VALUE '  '.                  
019700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
019900     88  IMS-EJ-OK                           VALUE 'XD'.                  
020000                                                                          
020100 01  GODK-STATUSKODER.                                                    
020200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020300                                                                          
020400 01  SSA1                        PIC X(64).                               
020500 01  SSA2                        PIC X(64).                               
020600 01  SSA3                        PIC X(64).                               
020700     EJECT                                                                
020800*    --- IMS FUNKTIONSKODER                                               
020900*01  -COPY W0003                                                          
021000     EJECT                                                                
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
021200 01  DLI-IO-WDGX6322.                                                     
021300*    03  -COPY WDGX6322                                                   
021400     EJECT                                                                
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
021600 01  DLI-IO-WDGX6324.                                                     
021700*    03  -COPY WDGX6324                                                   
021800                                                                          
021900     EJECT                                                                
022000*    ---  MSG INPUT-OUTPUT AREA                                           
022100*01  -COPY WMSGAREA                                                       
022200     EJECT                                                                
022300 01  FILLER                 PIC X(16)   VALUE 'KOM-OHUV-AREA'.            
022400 01  OHUV-AREA.                                                           
022500*    03   -COPY W4I25101   -PRE OHUV-                                     
022600     EJECT                                                                
022700 01  FILLER                 PIC X(16)   VALUE 'KOM-RAD-AREA'.             
022800 01  ORAD-AREA.                                                           
022900*    05   -COPY W4I25201   -PRE ORAD-                                     
023000     EJECT                                                                
023100*    --- AREOR FÖR W006KOM SUBMODUL                                       
023200 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
023300*01  -COPY WMSGKOM                                                        
023400     EJECT                                                                
023500 LINKAGE SECTION.                                                         
023600                                                                          
023700*01  -COPY W0009 -PRE MSG-                                                
023800     EJECT                                                                
023900*01  -COPY W0009 -PRE ALT-                                                
024000     EJECT                                                                
024100*01  -COPY W0009 -PRE KOMA-                                               
024200     EJECT                                                                
024300*01  -COPY W0008 -PRE 6321-                                               
024400     05  FILLER      PIC X.                                               
024500     EJECT                                                                
024600 01  ORDN-XXKP-PCB               PIC X.                                   
024700 01  ORDN-ORQL-PCB               PIC X.                                   
024800 01  ORDN-PROC-PCB               PIC X.                                   
024900 01  ORDN-ORQI-PCB               PIC X.                                   
025000     EJECT                                                                
025100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB 6321-PCB              
025200                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
025300                           ORDN-PROC-PCB ORDN-ORQI-PCB.                   
025400                                                                          
025500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB 6321-PCB              
025600                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
025700                           ORDN-PROC-PCB ORDN-ORQI-PCB.                   
025800                                                                          
025900                                                                          
026000     PERFORM A-INIT                                                       
026100     PERFORM S01-LAES-W216PP                                              
026200     DISPLAY PARM-AREA                                                    
026300                                                                          
026400*    IF PARM-FLKLAR = 'J'                                                 
026500        PERFORM B-SCRAP                                                   
026600*    ELSE                                                                 
026700*       IF PARM-FLKLAR = 'N'                                              
026800*          PERFORM C-SKAPA-ENSTAKA-SKROTORDER                             
026900*       END-IF                                                            
027000*    END-IF                                                               
027100                                                                          
027200     PERFORM Z-FINIT                                                      
027300     MOVE ZERO TO RETURN-CODE                                             
027400     GOBACK                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 A-INIT SECTION.                                                          
027800                                                                          
027900     OPEN INPUT W216PP                                                    
028000                                                                          
028100     ACCEPT DAGENS-DATUM FROM DATE                                        
028200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028300     PERFORM AA-NOLLSTALL-SORT-TAB                                        
028400     MOVE NEJ TO WS-OHUV-SKAPAD                                           
028500                 WS-ENSTAKA-SKROTORDER                                    
028600                 WS-TAB-UPD                                               
028700                 WS-TABESC-UPD                                            
028800     .                                                                    
028900     EJECT                                                                
029000 AA-NOLLSTALL-SORT-TAB SECTION.                                           
029100                                                                          
029200     MOVE +1 TO TAB-IX                                                    
029300     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
029400        MOVE SPACE TO TAB-IDDC(TAB-IX)                                    
029500        MOVE ZERO TO TAB-IDDISTR(TAB-IX)                                  
029600                     TAB-IDKUNDNR(TAB-IX)                                 
029700                     TAB-KDFRAKT(TAB-IX)                                  
029800                     TAB-IDARTNR(TAB-IX)                                  
029900                     TAB-DASKROT9-BEORD(TAB-IX)                           
030000        ADD +1 TO TAB-IX                                                  
030100     END-PERFORM                                                          
030200                                                                          
030300     MOVE +1 TO TAB2-IX                                                   
030400     PERFORM UNTIL TAB2-IX > TAB2-IX-MAX                                  
030500        MOVE SPACE TO TAB2-IDDC(TAB2-IX)                                  
030600                      TAB2-IDUSER(TAB2-IX)                                
030700        MOVE ZERO TO TAB2-IDDISTR(TAB2-IX)                                
030800                     TAB2-IDKUNDNR(TAB2-IX)                               
030900                     TAB2-KDFRAKT(TAB2-IX)                                
031000                     TAB2-IDARTNR(TAB2-IX)                                
031100                     TAB2-DASKROT9-BEORD(TAB2-IX)                         
031200        ADD +1 TO TAB2-IX                                                 
031300     END-PERFORM                                                          
031400     .                                                                    
031500     EJECT                                                                
031600 B-SCRAP SECTION.                                                         
031700                                                                          
031800     MOVE +1 TO PARM-IX                                                   
031900                TAB-IX                                                    
032000                TAB2-IX                                                   
032100                                                                          
032200     PERFORM UNTIL PARM-IX > PARM-IX-MAX                                  
032300        IF PARM-IDARTNR(PARM-IX) NUMERIC                                  
032400           MOVE PARM-KDARBTYP                TO W-KDARBTYP                
032500                                                W-6321-KDARBTYP           
032600           MOVE PARM-IDARTNR(PARM-IX)        TO W-IDARTNR                 
032700                                                W-6324-IDARTNR-MIN        
032800                                                W-6324-IDARTNR-MAX        
032900           MOVE PARM-IDDC(PARM-IX)           TO W-IDDC                    
033000                                                W-6324-IDDC-MIN           
033100                                                W-6324-IDDC-MAX           
033200           MOVE PARM-DASKROT9-BEORD(PARM-IX) TO W-DASKROT9                
033300           PERFORM IMS-GU-WDGX6324                                        
033400           IF SEGMENT-FINNS                                               
033500***          OBS ATT DET BARA SKAPAS EN TABELL/KÖRNING,                   
033600***          VI LÄGGER UPP ALLA AUTOMATSKROT + SOM TIDIGARE.              
033700***          SORTERING SKER EFTER DENNA PERFORM                           
033800             IF 6324-IDUSER = 'W2616800' OR 'W2169300'                    
033900                PERFORM S02-LAS-TILL-TABELL                               
034000                MOVE JA TO WS-TAB-UPD                                     
034100                ADD +1 TO TAB-IX                                          
034200             ELSE                                                         
034300               IF 6324-IDUSER = 'W2712D00'                                
034400               AND NOT (6324-IDKONTO   > ZERO                             
034500                     OR 6324-IDKST     > SPACE                            
034600                     OR (6324-IDANALYS NOT = SPACE))                      
034700                  PERFORM S02-LAS-TILL-TABELL                             
034800                  MOVE JA TO WS-TABESC-UPD                                
034900                  ADD +1 TO TAB2-IX                                       
035000               ELSE                                                       
035100                IF 6324-BELAGINS-DEL NOT = SPACE                          
035200                   MOVE JA TO WS-ENSTAKA-SKROTORDER                       
035300                   PERFORM S03-SKAPA-ORDERNR                              
035400                   PERFORM S99-SKAPA-SKROTORDER                           
035500                ELSE                                                      
035600                   IF 6324-IDKONTO > ZERO                                 
035700                   OR 6324-IDKST > SPACE                                  
035800                   OR (6324-IDANALYS NOT = SPACE)                         
035900                      MOVE JA TO WS-ENSTAKA-SKROTORDER                    
036000                      PERFORM S03-SKAPA-ORDERNR                           
036100                      PERFORM S99-SKAPA-SKROTORDER                        
036200                   ELSE                                                   
036300                      PERFORM S02-LAS-TILL-TABELL                         
036400                      MOVE JA TO WS-TAB-UPD                               
036500                      ADD +1 TO TAB-IX                                    
036600                   END-IF                                                 
036700                END-IF                                                    
036800               END-IF                                                     
036900             END-IF                                                       
037000           END-IF                                                         
037100        END-IF                                                            
037200        ADD +1 TO PARM-IX                                                 
037300     END-PERFORM                                                          
037400                                                                          
037500     IF WS-TAB-UPD = JA                                                   
037600       PERFORM BA-SKAPA-SKROTORDRAR                                       
037700     END-IF                                                               
037800                                                                          
037900     IF WS-TABESC-UPD = JA                                                
038000       PERFORM BB-SKAPA-SKROTORDRAR-ESC                                   
038100     END-IF                                                               
038200                                                                          
038300***TAR BORT SKROTORDER FRÅN BASEN                                         
038400     MOVE +1 TO PARM-IX                                                   
038500     PERFORM UNTIL PARM-IX > PARM-IX-MAX                                  
038600        IF PARM-IDARTNR(PARM-IX) NUMERIC                                  
038700           MOVE PARM-KDARBTYP                TO W-KDARBTYP                
038800                                                W-6321-KDARBTYP           
038900           MOVE PARM-IDARTNR(PARM-IX)        TO W-IDARTNR                 
039000                                      W-6324-IDARTNR-MIN                  
039100                                      W-6324-IDARTNR-MAX                  
039200           MOVE PARM-IDDC(PARM-IX)           TO W-IDDC                    
039300                                      W-6324-IDDC-MIN                     
039400                                      W-6324-IDDC-MAX                     
039500           MOVE PARM-DASKROT9-BEORD(PARM-IX) TO W-DASKROT9                
039600           PERFORM IMS-GU-WDGX6324                                        
039700           IF SEGMENT-FINNS                                               
039800              PERFORM IMS-DLET-WDGX6324                                   
039900           END-IF                                                         
040000           PERFORM S05-KOLLA-WDGX6322                                     
040100         END-IF                                                           
040200        ADD +1 TO PARM-IX                                                 
040300     END-PERFORM                                                          
040400     .                                                                    
040500     EJECT                                                                
040600 BA-SKAPA-SKROTORDRAR SECTION.                                            
040700     MOVE +24        TO STEGLAANGD                                        
040800     COMPUTE ANTAL = TAB-IX - 1                                           
040900     END-COMPUTE                                                          
041000     MOVE +11        TO NYCKELLAANGD                                      
041100     CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                      
041200     TAB-SORT-BEGREPP(1) NYCKELLAANGD                                     
041300                                                                          
041400     MOVE SPACE                   TO SPAR-IDDC                            
041500     MOVE ZERO                    TO SPAR-IDDISTR                         
041600                                  SPAR-IDKUNDNR                           
041700                                  SPAR-KDFRAKT                            
041800                                                                          
041900     MOVE ANTAL TO TAB-IX                                                 
042000     MOVE TAB-IX TO TAB-IX-MAX                                            
042100     MOVE +1 TO TAB-IX                                                    
042200     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
042300       IF TAB-IDARTNR(TAB-IX) NOT = ZERO                                  
042400                                                                          
042500          IF TAB-IDDC(TAB-IX)          = SPAR-IDDC                        
042600          AND TAB-IDDISTR(TAB-IX)      = SPAR-IDDISTR                     
042700          AND TAB-IDKUNDNR(TAB-IX)     = SPAR-IDKUNDNR                    
042800          AND TAB-KDFRAKT(TAB-IX)      = SPAR-KDFRAKT                     
042900                                                                          
043000             MOVE PARM-KDARBTYP                 TO W-KDARBTYP             
043100                                                W-6321-KDARBTYP           
043200             MOVE TAB-IDARTNR(TAB-IX)           TO W-IDARTNR              
043300                                        W-6324-IDARTNR-MIN                
043400                                        W-6324-IDARTNR-MAX                
043500             MOVE TAB-IDDC(TAB-IX)              TO W-IDDC                 
043600                                        W-6324-IDDC-MIN                   
043700                                        W-6324-IDDC-MAX                   
043800             MOVE TAB-DASKROT9-BEORD(TAB-IX) TO W-DASKROT9                
043900             PERFORM IMS-GU-WDGX6324                                      
044000             IF SEGMENT-FINNS                                             
044100                ADD +1 TO ORAD-IX                                         
044200                PERFORM S993-EDIT-TRANS-ORDERRADER                        
044300                IF TAB-IX = 12                                            
044400                OR TAB-IDDC (TAB-IX + 1) = SPACE                          
044500                  MOVE 'J'              TO ORAD-MID-FLSLUT                
044600                END-IF                                                    
044700* OBS! RAD-TABELLEN I W4I25201 ÄR SÄNKT TILL 6! DÄRFÖR MÅSTE MAN          
044800*   HÄR BRYTA EFTER 6 RADER OCH ANROPA W006KOM.                           
044900                IF ORAD-IX = ORAD-IX-MAX                                  
045000                  MOVE ORAD-AREA TO MSG-MID-OUT                           
045100                  DISPLAY ORAD-AREA                                       
045200                  PERFORM S04-SKICKA-TRANS                                
045300                  MOVE +1       TO ORAD-IX                                
045400                  PERFORM     UNTIL ORAD-IX > ORAD-IX-MAX                 
045500                    MOVE SPACE TO ORAD-MID-RADER(ORAD-IX)                 
045600                    ADD +1      TO ORAD-IX                                
045700                  END-PERFORM                                             
045800                  MOVE +0       TO ORAD-IX                                
045900                END-IF                                                    
046000             END-IF                                                       
046100          ELSE                                                            
046200             IF SPAR-IDDC = SPACE                                         
046300             AND SPAR-IDDISTR = ZERO                                      
046400             AND SPAR-IDKUNDNR = ZERO                                     
046500             AND SPAR-KDFRAKT = ZERO                                      
046600                MOVE PARM-KDARBTYP          TO W-KDARBTYP                 
046700                                            W-6321-KDARBTYP               
046800                MOVE TAB-IDARTNR(TAB-IX) TO W-IDARTNR                     
046900                                   W-6324-IDARTNR-MIN                     
047000                                   W-6324-IDARTNR-MAX                     
047100                MOVE TAB-IDDC(TAB-IX)       TO W-IDDC                     
047200                                   W-6324-IDDC-MIN                        
047300                                   W-6324-IDDC-MAX                        
047400                MOVE TAB-DASKROT9-BEORD(TAB-IX) TO W-DASKROT9             
047500                PERFORM IMS-GU-WDGX6324                                   
047600                IF SEGMENT-FINNS                                          
047700                   MOVE ZERO TO ORAD-IX                                   
047800                   PERFORM S03-SKAPA-ORDERNR                              
047900                   PERFORM S991-SKAPA-TRANS-ORDERHUVUD                    
048000                   ADD +1 TO ORAD-IX                                      
048100                   PERFORM S992-SKAPA-HUVUD-ORDERRADER                    
048200                   PERFORM S993-EDIT-TRANS-ORDERRADER                     
048300                   IF TAB-IX = 12                                         
048400                   OR TAB-IDDC (TAB-IX + 1) = SPACE                       
048500                     MOVE 'J'              TO ORAD-MID-FLSLUT             
048600                   END-IF                                                 
048700                                                                          
048800                   MOVE TAB-IDDC(TAB-IX)        TO SPAR-IDDC              
048900                   MOVE TAB-IDDISTR(TAB-IX)     TO SPAR-IDDISTR           
049000                   MOVE TAB-IDKUNDNR(TAB-IX) TO SPAR-IDKUNDNR             
049100                   MOVE TAB-KDFRAKT(TAB-IX)     TO SPAR-KDFRAKT           
049200                END-IF                                                    
049300             ELSE                                                         
049400                MOVE JA TO ORAD-MID-FLSLUT                                
049500                MOVE ORAD-AREA TO MSG-MID-OUT                             
049600                DISPLAY ORAD-AREA                                         
049700                PERFORM S04-SKICKA-TRANS                                  
049800                                                                          
049900                MOVE PARM-KDARBTYP          TO W-KDARBTYP                 
050000                                            W-6321-KDARBTYP               
050100                MOVE TAB-IDARTNR(TAB-IX)     TO W-IDARTNR                 
050200                                   W-6324-IDARTNR-MIN                     
050300                                   W-6324-IDARTNR-MAX                     
050400                MOVE TAB-IDDC(TAB-IX)       TO W-IDDC                     
050500                                   W-6324-IDDC-MIN                        
050600                                   W-6324-IDDC-MAX                        
050700                MOVE TAB-DASKROT9-BEORD(TAB-IX) TO W-DASKROT9             
050800                PERFORM IMS-GU-WDGX6324                                   
050900                IF SEGMENT-FINNS                                          
051000                   MOVE ZERO TO ORAD-IX                                   
051100                   PERFORM S03-SKAPA-ORDERNR                              
051200                   PERFORM S991-SKAPA-TRANS-ORDERHUVUD                    
051300                   ADD +1 TO ORAD-IX                                      
051400                   PERFORM S992-SKAPA-HUVUD-ORDERRADER                    
051500                   PERFORM S993-EDIT-TRANS-ORDERRADER                     
051600                   IF TAB-IX = 12                                         
051700                   OR TAB-IDDC (TAB-IX + 1) = SPACE                       
051800                     MOVE 'J'              TO ORAD-MID-FLSLUT             
051900                   END-IF                                                 
052000                                                                          
052100                   MOVE TAB-IDDC(TAB-IX)        TO SPAR-IDDC              
052200                   MOVE TAB-IDDISTR(TAB-IX)     TO SPAR-IDDISTR           
052300                   MOVE TAB-IDKUNDNR(TAB-IX) TO SPAR-IDKUNDNR             
052400                   MOVE TAB-KDFRAKT(TAB-IX)     TO SPAR-KDFRAKT           
052500                END-IF                                                    
052600             END-IF                                                       
052700          END-IF                                                          
052800       END-IF                                                             
052900       ADD +1 TO TAB-IX                                                   
053000     END-PERFORM                                                          
053100     MOVE JA TO ORAD-MID-FLSLUT                                           
053200     IF ORAD-IX > +0                                                      
053300       MOVE ORAD-AREA TO MSG-MID-OUT                                      
053400       DISPLAY ORAD-AREA                                                  
053500       PERFORM S04-SKICKA-TRANS                                           
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900 BB-SKAPA-SKROTORDRAR-ESC SECTION.                                        
054000     MOVE +24        TO STEGLAANGD                                        
054100     COMPUTE ANTAL = TAB2-IX - 1                                          
054200     END-COMPUTE                                                          
054300     MOVE +11        TO NYCKELLAANGD                                      
054400     CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                      
054500     TAB2-SORT-BEGREPP(1) NYCKELLAANGD                                    
054600                                                                          
054700     MOVE SPACE                   TO SPAR2-IDDC                           
054800     MOVE ZERO                    TO SPAR2-IDDISTR                        
054900                                  SPAR2-IDKUNDNR                          
055000                                  SPAR2-KDFRAKT                           
055100                                                                          
055200     MOVE ANTAL TO TAB2-IX                                                
055300     MOVE TAB2-IX TO TAB2-IX-MAX                                          
055400     MOVE +1 TO TAB2-IX                                                   
055500     PERFORM UNTIL TAB2-IX > TAB2-IX-MAX                                  
055600       IF TAB2-IDARTNR(TAB2-IX) NOT = ZERO                                
055700                                                                          
055800          IF TAB2-IDDC(TAB2-IX)         = SPAR2-IDDC                      
055900          AND TAB2-IDDISTR(TAB2-IX)     = SPAR2-IDDISTR                   
056000          AND TAB2-IDKUNDNR(TAB2-IX)    = SPAR2-IDKUNDNR                  
056100          AND TAB2-KDFRAKT(TAB2-IX)     = SPAR2-KDFRAKT                   
056200                                                                          
056300             MOVE PARM-KDARBTYP                 TO W-KDARBTYP             
056400                                                W-6321-KDARBTYP           
056500             MOVE TAB2-IDARTNR(TAB2-IX)          TO W-IDARTNR             
056600                                        W-6324-IDARTNR-MIN                
056700                                        W-6324-IDARTNR-MAX                
056800             MOVE TAB2-IDDC(TAB2-IX)             TO W-IDDC                
056900                                        W-6324-IDDC-MIN                   
057000                                        W-6324-IDDC-MAX                   
057100             MOVE TAB2-DASKROT9-BEORD(TAB2-IX) TO W-DASKROT9              
057200             PERFORM IMS-GU-WDGX6324                                      
057300             IF SEGMENT-FINNS                                             
057400                ADD +1 TO ORAD-IX                                         
057500                PERFORM S993-EDIT-TRANS-ORDERRADER                        
057600                IF TAB2-IX = 12                                           
057700                OR TAB2-IDDC (TAB2-IX + 1) = SPACE                        
057800                  MOVE 'J'              TO ORAD-MID-FLSLUT                
057900                END-IF                                                    
058000* OBS! RAD-TABELLEN I W4I25201 ÄR SÄNKT TILL 6! DÄRFÖR MÅSTE MAN          
058100*   HÄR BRYTA EFTER 6 RADER OCH ANROPA W006KOM.                           
058200                IF ORAD-IX = ORAD-IX-MAX                                  
058300                  MOVE ORAD-AREA TO MSG-MID-OUT                           
058400                  DISPLAY ORAD-AREA                                       
058500                  PERFORM S04-SKICKA-TRANS                                
058600                  MOVE +1       TO ORAD-IX                                
058700                  PERFORM     UNTIL ORAD-IX > ORAD-IX-MAX                 
058800                    MOVE SPACE TO ORAD-MID-RADER(ORAD-IX)                 
058900                    ADD +1      TO ORAD-IX                                
059000                  END-PERFORM                                             
059100                  MOVE +0       TO ORAD-IX                                
059200                END-IF                                                    
059300             END-IF                                                       
059400          ELSE                                                            
059500             IF SPAR2-IDDC = SPACE                                        
059600             AND SPAR2-IDDISTR = ZERO                                     
059700             AND SPAR2-IDKUNDNR = ZERO                                    
059800             AND SPAR2-KDFRAKT = ZERO                                     
059900                MOVE PARM-KDARBTYP          TO W-KDARBTYP                 
060000                                            W-6321-KDARBTYP               
060100                MOVE TAB2-IDARTNR(TAB2-IX) TO W-IDARTNR                   
060200                                   W-6324-IDARTNR-MIN                     
060300                                   W-6324-IDARTNR-MAX                     
060400                MOVE TAB2-IDDC(TAB2-IX)      TO W-IDDC                    
060500                                   W-6324-IDDC-MIN                        
060600                                   W-6324-IDDC-MAX                        
060700                MOVE TAB2-DASKROT9-BEORD(TAB2-IX) TO W-DASKROT9           
060800                PERFORM IMS-GU-WDGX6324                                   
060900                IF SEGMENT-FINNS                                          
061000                   MOVE ZERO TO ORAD-IX                                   
061100                   PERFORM S03-SKAPA-ORDERNR                              
061200                   PERFORM S991-SKAPA-TRANS-ORDERHUVUD                    
061300                   ADD +1 TO ORAD-IX                                      
061400                   PERFORM S992-SKAPA-HUVUD-ORDERRADER                    
061500                   PERFORM S993-EDIT-TRANS-ORDERRADER                     
061600                   IF TAB2-IX = 12                                        
061700                   OR TAB2-IDDC (TAB2-IX + 1) = SPACE                     
061800                     MOVE 'J'              TO ORAD-MID-FLSLUT             
061900                   END-IF                                                 
062000                                                                          
062100                   MOVE TAB2-IDDC(TAB2-IX)       TO SPAR2-IDDC            
062200                   MOVE TAB2-IDDISTR(TAB2-IX)    TO SPAR2-IDDISTR         
062300                   MOVE TAB2-IDKUNDNR(TAB2-IX) TO SPAR2-IDKUNDNR          
062400                   MOVE TAB2-KDFRAKT(TAB2-IX)    TO SPAR2-KDFRAKT         
062500                END-IF                                                    
062600             ELSE                                                         
062700                MOVE JA TO ORAD-MID-FLSLUT                                
062800                MOVE ORAD-AREA TO MSG-MID-OUT                             
062900                DISPLAY ORAD-AREA                                         
063000                PERFORM S04-SKICKA-TRANS                                  
063100                                                                          
063200                MOVE PARM-KDARBTYP          TO W-KDARBTYP                 
063300                                            W-6321-KDARBTYP               
063400                MOVE TAB2-IDARTNR(TAB2-IX)    TO W-IDARTNR                
063500                                   W-6324-IDARTNR-MIN                     
063600                                   W-6324-IDARTNR-MAX                     
063700                MOVE TAB2-IDDC(TAB2-IX)      TO W-IDDC                    
063800                                   W-6324-IDDC-MIN                        
063900                                   W-6324-IDDC-MAX                        
064000                MOVE TAB2-DASKROT9-BEORD(TAB2-IX) TO W-DASKROT9           
064100                PERFORM IMS-GU-WDGX6324                                   
064200                IF SEGMENT-FINNS                                          
064300                   MOVE ZERO TO ORAD-IX                                   
064400                   PERFORM S03-SKAPA-ORDERNR                              
064500                   PERFORM S991-SKAPA-TRANS-ORDERHUVUD                    
064600                   ADD +1 TO ORAD-IX                                      
064700                   PERFORM S992-SKAPA-HUVUD-ORDERRADER                    
064800                   PERFORM S993-EDIT-TRANS-ORDERRADER                     
064900                   IF TAB2-IX = 12                                        
065000                   OR TAB2-IDDC (TAB2-IX + 1) = SPACE                     
065100                     MOVE 'J'              TO ORAD-MID-FLSLUT             
065200                   END-IF                                                 
065300                                                                          
065400                   MOVE TAB2-IDDC(TAB2-IX)       TO SPAR2-IDDC            
065500                   MOVE TAB2-IDDISTR(TAB2-IX)    TO SPAR2-IDDISTR         
065600                   MOVE TAB2-IDKUNDNR(TAB2-IX) TO SPAR2-IDKUNDNR          
065700                   MOVE TAB2-KDFRAKT(TAB2-IX)    TO SPAR2-KDFRAKT         
065800                END-IF                                                    
065900             END-IF                                                       
066000          END-IF                                                          
066100       END-IF                                                             
066200       ADD +1 TO TAB2-IX                                                  
066300     END-PERFORM                                                          
066400     MOVE JA TO ORAD-MID-FLSLUT                                           
066500     IF ORAD-IX > +0                                                      
066600       MOVE ORAD-AREA TO MSG-MID-OUT                                      
066700       DISPLAY ORAD-AREA                                                  
066800       PERFORM S04-SKICKA-TRANS                                           
066900     END-IF                                                               
067000     .                                                                    
067100     EJECT                                                                
067200*C-SKAPA-ENSTAKA-SKROTORDER SECTION.                                      
067300*                                                                         
067400*    MOVE +1 TO PARM-IX                                                   
067500*               TAB-IX                                                    
067600*    PERFORM UNTIL PARM-IX > PARM-IX-MAX                                  
067700*       IF PARM-IDARTNR(PARM-IX) NUMERIC                                  
067800*          MOVE PARM-KDARBTYP                TO W-KDARBTYP                
067900*                                               W-6321-KDARBTYP           
068000*          MOVE PARM-IDARTNR(PARM-IX)        TO W-IDARTNR                 
068100*                                             W-6324-IDARTNR-MAX          
068200*                                             W-6324-IDARTNR-MIN          
068300*          MOVE PARM-IDDC(PARM-IX)           TO W-IDDC                    
068400*                                             W-6324-IDDC-MAX             
068500*                                             W-6324-IDDC-MIN             
068600*          MOVE PARM-DASKROT9-BEORD(PARM-IX) TO W-DASKROT9                
068700*                                                                         
068800*          PERFORM IMS-GU-WDGX6324                                        
068900*          IF SEGMENT-FINNS                                               
069000*             MOVE JA TO WS-ENSTAKA-SKROTORDER                            
069100*             PERFORM S03-SKAPA-ORDERNR                                   
069200*             PERFORM S99-SKAPA-SKROTORDER                                
069300*          END-IF                                                         
069400*       END-IF                                                            
069500*       ADD +1 TO PARM-IX                                                 
069600*    END-PERFORM                                                          
069700*                                                                         
069800*    MOVE +1 TO PARM-IX                                                   
069900*    PERFORM UNTIL PARM-IX > PARM-IX-MAX                                  
070000*       IF PARM-IDARTNR(PARM-IX) NUMERIC                                  
070100*          MOVE PARM-KDARBTYP                TO W-KDARBTYP                
070200*                                               W-6321-KDARBTYP           
070300*          MOVE PARM-IDARTNR(PARM-IX)        TO W-IDARTNR                 
070400*                                             W-6324-IDARTNR-MAX          
070500*                                             W-6324-IDARTNR-MIN          
070600*          MOVE PARM-IDDC(PARM-IX)           TO W-IDDC                    
070700*                                             W-6324-IDDC-MAX             
070800*                                             W-6324-IDDC-MIN             
070900*          MOVE PARM-DASKROT9-BEORD(PARM-IX) TO W-DASKROT9                
071000*                                                                         
071100*          PERFORM IMS-GU-WDGX6324                                        
071200*          IF SEGMENT-FINNS                                               
071300*             PERFORM IMS-DLET-WDGX6324                                   
071400*          END-IF                                                         
071500*          PERFORM S05-KOLLA-WDGX6322                                     
071600*       END-IF                                                            
071700*       ADD +1 TO PARM-IX                                                 
071800*    END-PERFORM                                                          
071900*    .                                                                    
072000*    EJECT                                                                
072100 Z-FINIT SECTION.                                                         
072200                                                                          
072300     CLOSE W216PP                                                         
072400     MOVE 'S' TO POSTSUM-OPKOD                                            
072500     CALL POSTSUM USING POSTSUM-PARM                                      
072600     .                                                                    
072700     EJECT                                                                
072800 S01-LAES-W216PP SECTION.                                                 
072900                                                                          
073000     READ W216PP INTO IN-AREA1                                            
073100     MOVE IN-FLKLAR   TO PARM-FLKLAR                                      
073200     MOVE IN-KDARBTYP TO PARM-KDARBTYP                                    
073300                                                                          
073400     READ W216PP INTO PARM-TAB-RAD(1)                                     
073500     READ W216PP INTO PARM-TAB-RAD(2)                                     
073600     READ W216PP INTO PARM-TAB-RAD(3)                                     
073700     READ W216PP INTO PARM-TAB-RAD(4)                                     
073800     READ W216PP INTO PARM-TAB-RAD(5)                                     
073900     READ W216PP INTO PARM-TAB-RAD(6)                                     
074000     READ W216PP INTO PARM-TAB-RAD(7)                                     
074100     READ W216PP INTO PARM-TAB-RAD(8)                                     
074200     READ W216PP INTO PARM-TAB-RAD(9)                                     
074300     READ W216PP INTO PARM-TAB-RAD(10)                                    
074400     READ W216PP INTO PARM-TAB-RAD(11)                                    
074500     READ W216PP INTO PARM-TAB-RAD(12)                                    
074600     .                                                                    
074700     EJECT                                                                
074800 S02-LAS-TILL-TABELL SECTION.                                             
074900                                                                          
075000     IF 6324-IDUSER = 'W2712D00'                                          
075100       MOVE PARM-IDDC(PARM-IX) TO TAB2-IDDC(TAB2-IX)                      
075200       MOVE PARM-DASKROT9-BEORD(PARM-IX)                                  
075300                               TO TAB2-DASKROT9-BEORD(TAB2-IX)            
075400       MOVE PARM-IDARTNR(PARM-IX)                                         
075500                               TO TAB2-IDARTNR(TAB2-IX)                   
075600       MOVE 6324-IDDISTR     TO TAB2-IDDISTR(TAB2-IX)                     
075700       MOVE 6324-IDKUNDNR    TO TAB2-IDKUNDNR(TAB2-IX)                    
075800       MOVE 6324-KDFRAKT     TO TAB2-KDFRAKT(TAB2-IX)                     
075900     ELSE                                                                 
076000       MOVE PARM-IDDC(PARM-IX) TO TAB-IDDC(TAB-IX)                        
076100       MOVE PARM-DASKROT9-BEORD(PARM-IX)                                  
076200                               TO TAB-DASKROT9-BEORD(TAB-IX)              
076300       MOVE PARM-IDARTNR(PARM-IX)                                         
076400                               TO TAB-IDARTNR(TAB-IX)                     
076500       MOVE 6324-IDDISTR     TO TAB-IDDISTR(TAB-IX)                       
076600       MOVE 6324-IDKUNDNR    TO TAB-IDKUNDNR(TAB-IX)                      
076700       MOVE 6324-KDFRAKT     TO TAB-KDFRAKT(TAB-IX)                       
076800     END-IF                                                               
076900     .                                                                    
077000     EJECT                                                                
077100 S03-SKAPA-ORDERNR SECTION.                                               
077200                                                                          
077300     MOVE 'W216'         TO ORDN-IDSYSTEM                                 
077400     MOVE 6324-IDDISTR   TO ORDN-IDDISTR                                  
077500     MOVE 6324-IDKUNDNR  TO ORDN-IDKUNDNR                                 
077600     MOVE ZERO           TO ORDN-IDORDNR-IN                               
077700                                                                          
077800     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
077900                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
078000                                                                          
078100     MOVE ORDN-IDORDNR-UT TO WS-IDORDNR-NUM                               
078200     .                                                                    
078300     EJECT                                                                
078400 S04-SKICKA-TRANS SECTION.                                                
078500                                                                          
078600     CALL W006KOM USING MSG-PCB                                           
078700                        ALT-PCB                                           
078800                        KOMA-PCB                                          
078900                        MSG-KOM-WMSGKOM                                   
079000                        MSG-IO-AREA                                       
079100     .                                                                    
079200     EJECT                                                                
079300 S05-KOLLA-WDGX6322 SECTION.                                              
079400                                                                          
079500     PERFORM IMS-GET-WDGX6322                                             
079600     IF SEGMENT-FINNS                                                     
079700        PERFORM IMS-GET-WDGX6324                                          
079800        IF SEGMENT-SAKNAS                                                 
079900           PERFORM IMS-GET-WDGX6322                                       
080000           PERFORM IMS-DLET-WDGX6322                                      
080100        END-IF                                                            
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500 S99-SKAPA-SKROTORDER SECTION.                                            
080600                                                                          
080700     IF WS-ENSTAKA-SKROTORDER = JA                                        
080800        MOVE 1 TO ORAD-IX                                                 
080900        PERFORM S991-SKAPA-TRANS-ORDERHUVUD                               
081000        PERFORM S992-SKAPA-HUVUD-ORDERRADER                               
081100        PERFORM S993-EDIT-TRANS-ORDERRADER                                
081200        MOVE JA TO ORAD-MID-FLSLUT                                        
081300        MOVE ORAD-AREA TO MSG-MID-OUT                                     
081400        PERFORM S04-SKICKA-TRANS                                          
081500     END-IF                                                               
081600     .                                                                    
081700     EJECT                                                                
081800 S991-SKAPA-TRANS-ORDERHUVUD SECTION.                                     
081900                                                                          
082000     MOVE SPACE         TO MSG-KOM-WMSGKOM                                
082100     MOVE +54           TO MSG-KOM-KVLL                                   
082200     MOVE LOW-VALUE     TO MSG-KOM-KDZ1                                   
082300                           MSG-KOM-KDZ2                                   
082400     MOVE SPACE         TO MSG-KOM-KDTRANS                                
082500     MOVE 'W4I25101'    TO MSG-KOM-IDCPYTXT                               
082600     MOVE 'SKROT   '    TO MSG-KOM-IDSNDNOD                               
082700     IF 6324-KDSTASKR = 1                                                 
082800        MOVE 'W6032200'    TO MSG-KOM-IDSNDJOB                            
082900     ELSE                                                                 
083000        MOVE 'W6032300'    TO MSG-KOM-IDSNDJOB                            
083100     END-IF                                                               
083200     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
083300     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
083400     MOVE SPACE         TO MSG-KOM-IDMFSMED                               
083500                           MSG-KOM-KDSVAR                                 
083600                                                                          
083700     MOVE LENGTH OF OHUV-MID-W4I25101 TO MSG-KVLL                         
083800     ADD +17                TO MSG-KVLL                                   
083900     MOVE LOW-VALUE         TO MSG-KDZ1                                   
084000                               MSG-KDZ2                                   
084100     MOVE 'W4T251X'         TO MSG-KDTRANS-1                              
084200     MOVE '4251'            TO MSG-IDTRANS-1                              
084300     MOVE '1'               TO MSG-KDMFSFOR-1                             
084400                                                                          
084500     MOVE SPACE             TO OHUV-MID-W4I25101                          
084600     MOVE 6324-IDDISTR      TO TEST-IDDISTR                               
084700     IF DIST18-SCRAP-NDC OR DIST18-SCRAP-NDC-SC-LOCAL                     
084800       MOVE 'REFT'          TO OHUV-MID-IDSYSTEM                          
084900     ELSE                                                                 
085000***  MOVE 'REFT'            TO OHUV-MID-IDSYSTEM                          
085100       MOVE 'W216'          TO OHUV-MID-IDSYSTEM                          
085200     END-IF                                                               
085300     MOVE 6324-IDDISTR      TO WS-IDDISTR-NUM                             
085400     MOVE WS-IDDISTR        TO OHUV-MID-IDDISTR                           
085500     MOVE 6324-IDKUNDNR     TO WS-IDKUNDNR-NUM                            
085600                               KUND17-IDKUNDNR                            
085700     MOVE WS-IDKUNDNR       TO OHUV-MID-IDKUNDNR                          
085800     MOVE WS-IDORDNR        TO OHUV-MID-IDORDNR                           
085900     MOVE 6324-KDORDKL      TO WS-KDORDKL-DISPLAY                         
086000     MOVE WS-KDORDKL-DISPLAY    TO OHUV-MID-KDORDKL                       
086100     MOVE 6324-BELAGINS-DEL TO OHUV-MID-BELAGINS                          
086200     IF 6324-KDFRAKT = ZERO                                               
086300        MOVE SPACE          TO OHUV-MID-KDFRAKT                           
086400     ELSE                                                                 
086500        MOVE 6324-KDFRAKT   TO WS-KDFRAKT-DISPLAY                         
086600        MOVE WS-KDFRAKT-DISPLAY  TO OHUV-MID-KDFRAKT                      
086700     END-IF                                                               
086800     IF 6324-IDKONTO = ZERO                                               
086900        MOVE SPACE          TO OHUV-MID-IDKONTO                           
087000     ELSE                                                                 
087100        MOVE 6324-IDKONTO TO WS-IDKONTO-DISPLAY                           
087200        MOVE WS-IDKONTO-DISPLAY TO OHUV-MID-IDKONTO                       
087300                                                                          
087400        MOVE W-IDDC   TO WS-IDDC                                          
087410        EVALUATE TRUE                                                     
087500           WHEN NDC-CN OR LDC-CN                                          
087600              MOVE WC-IDFTG-CN    TO OHUV-MID-IDFTG                       
087800           WHEN NDC-US                                                    
087900              MOVE WC-IDFTG-US    TO OHUV-MID-IDFTG                       
088100           WHEN NDC-CA                                                    
088200              MOVE WC-IDFTG-CA    TO OHUV-MID-IDFTG                       
088201           WHEN NDC-BR                                                    
088202              MOVE WC-IDFTG-BR    TO OHUV-MID-IDFTG                       
088203           WHEN NDC-MX                                                    
088204              MOVE WC-IDFTG-MX    TO OHUV-MID-IDFTG                       
088210           WHEN NDC-IN                                                    
088220              MOVE WC-IDFTG-IN    TO OHUV-MID-IDFTG                       
088221           WHEN NDC-TH                                                    
088222              MOVE WC-IDFTG-TH    TO OHUV-MID-IDFTG                       
088223           WHEN NDC-TW                                                    
088224              MOVE WC-IDFTG-TW    TO OHUV-MID-IDFTG                       
088230           WHEN NDC-KR                                                    
088240              MOVE WC-IDFTG-KR    TO OHUV-MID-IDFTG                       
088250           WHEN NDC-MY                                                    
088260              MOVE WC-IDFTG-MY    TO OHUV-MID-IDFTG                       
088270           WHEN NDC-ZA                                                    
088280              MOVE WC-IDFTG-ZA    TO OHUV-MID-IDFTG                       
088290           WHEN NDC-TR                                                    
088291              MOVE WC-IDFTG-TR    TO OHUV-MID-IDFTG                       
088292           WHEN NDC-AE                                                    
088293              MOVE WC-IDFTG-AE    TO OHUV-MID-IDFTG                       
088300           WHEN OTHER                                                     
088400              MOVE WC-IDFTG-PV    TO OHUV-MID-IDFTG                       
088701        END-EVALUATE                                                      
088711*USA HAR '57' TILLS MAN LÖST BOKNING I FINANS                             
088712        IF DIST18-SCRAP-NDC-QUAL                                          
088713          MOVE WC-IDFTG-PV        TO OHUV-MID-IDFTG                       
088714        END-IF                                                            
088900     END-IF                                                               
089000     MOVE SPACE             TO OHUV-MID-IDKST                             
089100     MOVE 6324-IDANALYS     TO OHUV-MID-IDANALYS                          
089200     MOVE W-IDDC            TO OHUV-MID-IDDC                              
089300     MOVE JA                TO OHUV-MID-FLAUTFAK                          
089400     IF DIST18-SKROT-LDC AND                                              
089500        KUND17-SKROT                                                      
089600       MOVE NEJ            TO OHUV-MID-FLAUTFAK                           
089700     END-IF                                                               
089800     IF DIST18-SCRAP-NDC                                                  
089900       MOVE NEJ             TO OHUV-MID-FLAUTFAK                          
090000     END-IF                                                               
090100     MOVE NEJ               TO OHUV-MID-FLAUTPAC                          
090200                               OHUV-MID-FLEMBORD                          
090300                               OHUV-MID-FLOVRLEV                          
090400                               OHUV-MID-FLFORBI                           
090500                               OHUV-MID-FLORDTIL                          
090600     MOVE ZERO              TO OHUV-MID-TIREPDAT                          
090800                                                                          
090900     MOVE JA TO WS-OHUV-SKAPAD                                            
091000     MOVE OHUV-AREA TO MSG-MID-OUT                                        
091100     DISPLAY OHUV-AREA                                                    
091200     PERFORM S04-SKICKA-TRANS                                             
091300     .                                                                    
091400     EJECT                                                                
091500 S992-SKAPA-HUVUD-ORDERRADER SECTION.                                     
091600                                                                          
091700     MOVE 'W4I25201'    TO MSG-KOM-IDCPYTXT                               
091800     MOVE LENGTH OF ORAD-MID-W4I25201 TO MSG-KVLL                         
091900     ADD +17            TO MSG-KVLL                                       
092000     MOVE LOW-VALUE     TO MSG-KDZ1                                       
092100                           MSG-KDZ2                                       
092200     MOVE 'W4T252X'     TO MSG-KDTRANS-1                                  
092300     MOVE '4252'        TO MSG-IDTRANS-1                                  
092400     MOVE '1'           TO MSG-KDMFSFOR-1                                 
092500                                                                          
092600     MOVE SPACE         TO ORAD-MID-W4I25201                              
092700     IF DIST18-SCRAP-NDC OR DIST18-SCRAP-NDC-SC-LOCAL                     
092800       MOVE 'REFT'          TO OHUV-MID-IDSYSTEM                          
092900     ELSE                                                                 
093000***  MOVE 'REFT'        TO ORAD-MID-IDSYSTEM                              
093100       MOVE 'W216'      TO ORAD-MID-IDSYSTEM                              
093200     END-IF                                                               
093300     MOVE WS-IDDISTR    TO ORAD-MID-IDDISTR                               
093400     MOVE WS-IDKUNDNR   TO ORAD-MID-IDKUNDNR                              
093500     MOVE WS-IDORDNR    TO ORAD-MID-IDORDNR                               
093600     MOVE SPACE         TO ORAD-MID-BEVOLREF                              
093700     MOVE 'N'           TO ORAD-MID-FLSLUT                                
093800     .                                                                    
093900     EJECT                                                                
094000 S993-EDIT-TRANS-ORDERRADER SECTION.                                      
094100                                                                          
094200     MOVE W-IDARTNR       TO ORAD-MID-IDARTNR(ORAD-IX)                    
094300                             REK-IDARTNR                                  
094400     MOVE 9               TO REK-LNGD                                     
094500     MOVE 0               TO REK-REKSIFFR                                 
094600     CALL W009KSIF        USING REK-IDARTNR                               
094700                                REK-LNGD                                  
094800                                REK-REKSIFFR                              
094900                                                                          
095000     MOVE REK-REKSIFFR       TO ORAD-MID-REKSIFFR  (ORAD-IX)              
095100     MOVE 6324-KVSKROT-BEORD TO WS-KVSKROT-BEORD                          
095200     MOVE WS-KVSKROT-BEORD   TO ORAD-MID-KVBEART   (ORAD-IX)              
095300     IF 6324-IDKONTO = ZERO                                               
095400        MOVE SPACE           TO ORAD-MID-IDKONTO   (ORAD-IX)              
095500     ELSE                                                                 
095600        MOVE WS-IDKONTO-DISPLAY TO ORAD-MID-IDKONTO (ORAD-IX)             
095700     END-IF                                                               
095800     MOVE 6324-IDKST         TO ORAD-MID-IDKST     (ORAD-IX)              
095900     MOVE '1'                TO ORAD-MID-KDKVBRYT  (ORAD-IX)              
096000                                                                          
096100     MOVE W-IDDC             TO WS-IDDC                                   
096200     IF PARM-KDARBTYP = 'QUAL'                                            
096300       IF LDC OR SDC-ES OR SDC-IT OR LDC-CN OR NDC-CN                     
096400         MOVE 'QALSCRAP'     TO ORAD-MID-BERADREF   (ORAD-IX)             
096500       END-IF                                                             
096600     ELSE                                                                 
096700       IF PARM-KDARBTYP = 'ESC'                                           
096800         IF LDC OR SDC-ES OR SDC-IT OR LDC-CN OR NDC-CN OR NDC-NA         
096900           MOVE 'ECOSCRAP'   TO ORAD-MID-BERADREF   (ORAD-IX)             
097000         END-IF                                                           
097100       END-IF                                                             
097200       IF PARM-KDARBTYP = 'ANSK'                                          
097300         IF LDC-CN OR NDC-CN OR NDC-US                                    
097400           MOVE 'ECOSCRAP'   TO ORAD-MID-BERADREF   (ORAD-IX)             
097500         END-IF                                                           
097600       END-IF                                                             
097700     END-IF                                                               
097800                                                                          
097900*    IF TAB-IX = 12                                                       
098000*    OR TAB-IDDC (TAB-IX + 1) = SPACE                                     
098100*      MOVE 'J'              TO ORAD-MID-FLSLUT                           
098200*    END-IF                                                               
098300                                                                          
098400     MOVE ORAD-AREA TO MSG-MID-OUT                                        
098500     .                                                                    
098600     EJECT                                                                
098700* IMS SECTIONER                                                           
098800                                                                          
098900 IMS-GU-WDGX6324 SECTION.                                                 
099000     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
099100          DELIMITED BY SIZE INTO SSA1                                     
099200     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X ')'                        
099300          DELIMITED BY SIZE INTO SSA2                                     
099400     STRING 'WDGX6324(KY6324  =>' W-KY6324-MIN-X                          
099500                    '&KY6324  =<' W-KY6324-MAX-X ')'                      
099600          DELIMITED BY SIZE INTO SSA3                                     
099700     MOVE '  GE' TO GODK-STATUSKODER                                      
099800     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2            
099900                           SSA3                                           
100000     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
100100     PERFORM IMS-STATUSKONTROLL                                           
100200     .                                                                    
100300     SKIP2                                                                
100400 IMS-DLET-WDGX6324 SECTION.                                               
100500     MOVE '  ' TO GODK-STATUSKODER                                        
100600     CALL CBLTDLI USING DLET 6321-PCB DLI-IO-WDGX6324                     
100700     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
100800     PERFORM IMS-STATUSKONTROLL                                           
100900     .                                                                    
101000     EJECT                                                                
101100 IMS-GET-WDGX6322 SECTION.                                                
101200     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
101300          DELIMITED BY SIZE INTO SSA1                                     
101400     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X ')'                        
101500          DELIMITED BY SIZE INTO SSA2                                     
101600     MOVE '  GE' TO GODK-STATUSKODER                                      
101700     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2            
101800     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
101900     PERFORM IMS-STATUSKONTROLL                                           
102000     .                                                                    
102100     SKIP2                                                                
102200 IMS-GET-WDGX6324 SECTION.                                                
102300     MOVE 'WDGX6324 ' TO SSA1                                             
102400     MOVE '  GE' TO GODK-STATUSKODER                                      
102500     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6324 SSA1                 
102600     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
102700     PERFORM IMS-STATUSKONTROLL                                           
102800     .                                                                    
102900     SKIP2                                                                
103000 IMS-DLET-WDGX6322 SECTION.                                               
103100     MOVE '  ' TO GODK-STATUSKODER                                        
103200     CALL CBLTDLI USING DLET 6321-PCB DLI-IO-WDGX6322                     
103300     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
103400     PERFORM IMS-STATUSKONTROLL                                           
103500     .                                                                    
103600     EJECT                                                                
103700 IMS-STATUSKONTROLL SECTION.                                              
103800     SET STATUS-IX TO 1                                                   
103900     SEARCH GODK-STATUS                                                   
104000       AT END                                                             
104100         MOVE 'IMS RETURKOD : ' TO FELTEXT-STR                            
104200         DISPLAY FELTEXT STATUS-WS                                        
104300         CALL FELLOG                                                      
104400       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
104500         CONTINUE                                                         
104600     END-SEARCH                                                           
104700     .                                                                    
