000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2210700.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   00/10/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER AVROP MED DATUM FÖREGÅENDE ARBETSDAG (SB),                 
000900*        GÖR DIVERSE KONTROLLER OCH VID BEHOV SKAPAR                      
001000*        LARMUNDERLAG (POSTER TILL W22108)                                
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDD9   SB                                  
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDR2                                       
001500*        PROGRAMMET LÄSER      W6D1                                       
001600*        PROGRAMMET LÄSER      WDK6                                       
001700*        PROGRAMMET LÄSER      WDD9                                       
001800*        PROGRAMMET LÄSER      WLXXBX (WDR2)                              
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- LARMPOSTER TILL W22108-PGM                                 
003300     SELECT W22108                     ASSIGN TO W22107D1.                
003400     SKIP2                                                                
003500*          --- LARMPOSTER TILL W22114-PGM                                 
003600     SELECT W22114                     ASSIGN TO W22107D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W22108                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY WDD401   -PRE  UT-  -L.                                   
004700     SKIP3                                                                
004800 FD  W22114                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  POST -COPY W22114   -PRE  UT2-  -L.                                  
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W2210700'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 01  SW-CDC                      PIC X       VALUE 'N'.                   
006200     88  CDC                                 VALUE 'J'.                   
006300 01  SW-EMIL                     PIC X       VALUE 'N'.                   
006400     88  EMIL                                VALUE 'J'.                   
006500 01  SW-IDLEVNR-ALARM            PIC X       VALUE 'N'.                   
006600     88  IDLEVNR-ALARM-OK                    VALUE 'J'.                   
006700 01  SW-AVIS                     PIC X       VALUE SPACE.                 
006800     88  AVIS                                VALUE 'J'.                   
006900 01  SW-WDF1                     PIC X       VALUE SPACE.                 
007000     88  WDF1-LAEST                          VALUE 'J'.                   
007100 01  SW-WDF1-FINNS               PIC X       VALUE SPACE.                 
007200     88  WDF1-FINNS                          VALUE 'J'.                   
007300 01  SW-IDLOPNRM                 PIC X       VALUE SPACE.                 
007400 01  SW-LARM                     PIC X       VALUE SPACE.                 
007500 01  SW-WDD906-SAKNAS            PIC X       VALUE SPACE.                 
007600 01  W1-DAREGDAT                 PIC 9(08).                               
007700 01  W1-DAREGDAT-9KOMPL          PIC 9(08).                               
007800 77  W1-TIKLOCK                  PIC 9(09).                               
007900 77  W1-TIKLOCK-9KOMPL           PIC 9(09)   COMP-3.                      
008000 01  SUM-KVAVIS                  PIC S9(7)   COMP-3.                      
008100 01  SUM-KVAVROP-AVB             PIC S9(7)   COMP-3.                      
008200 01  SUM-KVAVROP-BR              PIC S9(7)   COMP-3.                      
008300 01  SPAR-KVBR                   PIC S9(7)   COMP-3.                      
008400     EJECT                                                                
008500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009000 01  DAGENS-DATUM-AAVVD          PIC 9(5)    VALUE ZERO.                  
009100 01  FILLER REDEFINES DAGENS-DATUM-AAVVD.                                 
009200     03  DAGENS-DATUM-AA         PIC 9(2).                                
009300     03  DAGENS-DATUM-VV         PIC 9(2).                                
009400     03  DAGENS-DATUM-D          PIC 9(1).                                
009500 01  W-AAMMDD-SVAR               PIC 9(6)    VALUE ZERO.                  
009600 01  W-AAAAVV-SVAR               PIC 9(6)    VALUE ZERO.                  
009700 01  FILLER REDEFINES W-AAAAVV-SVAR.                                      
009800     03  W-AAAAVV-SE             PIC 9(2).                                
009900     03  W-AAAAVV-AA             PIC 9(2).                                
010000     03  W-AAAAVV-VV             PIC 9(2).                                
010100 01  W-TILEVDAG-SVAR             PIC 9(1)    VALUE ZERO.                  
010200 01  DAGENS-DATUM-MINUS-1        PIC 9(6)    VALUE ZERO.                  
010300 01  W-JUST-AAAAVVD              PIC 9(7)    VALUE ZERO.                  
010400 01  FILLER REDEFINES W-JUST-AAAAVVD.                                     
010500     03  W-JUST-DAAVROP          PIC 9(6).                                
010600     03  W-JUST-TILEVDAG         PIC 9(1).                                
010700 01  FILLER REDEFINES W-JUST-AAAAVVD.                                     
010800     03  FILLER                  PIC 9(2).                                
010900     03  W-JUST-AAVVD            PIC 9(5).                                
011000                                                                          
011100 01  WS-AAAAVVD                  PIC 9(7).                                
011200 01  FILLER   REDEFINES WS-AAAAVVD.                                       
011300     03  FILLER                  PIC 9(2).                                
011400     03  WS-AAVV                 PIC 9(4).                                
011500     03  WS-TILEVDAG             PIC 9(1).                                
011600 01  FILLER   REDEFINES WS-AAAAVVD.                                       
011700     03  WS-AAAAVV               PIC 9(6).                                
011800     03  FILLER                  PIC 9(1).                                
011900 01  FILLER   REDEFINES WS-AAAAVVD.                                       
012000     03  FILLER                  PIC 9(2).                                
012100     03  WS-AAVVD                PIC 9(5).                                
012200 01  WS-IDLOPNRM-PL              PIC 9(9).                                
012300 01  FILLER   REDEFINES WS-IDLOPNRM-PL.                                   
012400     03  WS-IDLOPNRM-AAVVD       PIC 9(5).                                
012500     03  FILLER                  PIC 9(4).                                
012600*SUPPLIER TO BE SKIPPED FOR ALARM                                         
012700 01 WS-IDLEVNR-ALARM             PIC X(5)  VALUE SPACE.                   
012800     88 NOT-GOOD-IDLEVNR                   VALUE 'AFGE9'.                 
012900                                                                          
013000*01  -COPY WWPRODSL                                                       
013100                                                                          
013200 01  SPAR-AREA.                                                           
013300*    03 -COPY WDD905    -PRE  SPAR-                                       
013400     EJECT                                                                
013500 01  DYNAMISKA-SUBPROGRAM.                                                
013600*                                                                         
013700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
014100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014300     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
014400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
014500     SKIP2                                                                
014600*    --- PARAMETRAR TILL ABEND                                            
014700                                                                          
014800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
015000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
015100     SKIP2                                                                
015200 01  FELTEXT.                                                             
015300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015500     EJECT                                                                
015600*01  -COPY WWDCKONS                                                       
015700     EJECT                                                                
015800*01  -COPY WDAGAREA                                                       
015900     EJECT                                                                
016000*01  -COPY WORKAREA                                                       
016100     EJECT                                                                
016200*    --- PARAMETRAR TILL DATKORT                                          
016300*                                                                         
016400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22107'.              
016500     SKIP2                                                                
016600 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
016700     SKIP2                                                                
016800*01  -COPY WDATKORT                                                       
016900     EJECT                                                                
017000*    --- PARAMETRAR TILL POSTSUM                                          
017100*                                                                         
017200*01  -COPY W0005   -PRE  POSTSUM-                                         
017300     EJECT                                                                
017400*01  -COPY WDATAREA                                                       
017500     EJECT                                                                
017600*    -COPY W200EMAB                                                       
017700     EJECT                                                                
017800 01  UT-AREA-START               PIC X(24)   VALUE                        
017900                                 'UT-AREA-START  '.                       
018000     SKIP2                                                                
018100                                                                          
018200*01  AREA -COPY WDD401       -PRE UT-                                     
018300     EJECT                                                                
018400 01  UT2-AREA-START              PIC X(24)   VALUE                        
018500                                 'UT2-AREA-START '.                       
018600     SKIP2                                                                
018700                                                                          
018800*01  AREA -COPY W22114       -PRE UT2-                                    
018900     EJECT                                                                
019000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019100*                                                                         
019200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019300     SKIP3                                                                
019400 01  NYCKLAR-TILL-DLI.                                                    
019500     03  W-IDARTNR-X.                                                     
019600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019700     03  W-IDLEVNR-X.                                                     
019800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
019900     03  W-WDD901KY-X.                                                    
020000         07  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
020100         07  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
020200     03  W-WDD905KY-X.                                                    
020300         05  W-DAAVROP-X.                                                 
020400             07  W-DAAVROP       PIC 9(6)    VALUE ZERO.                  
020500         05  W-TILEVDAG-X.                                                
020600             07  W-TILEVDAG      PIC S9(1)   VALUE ZERO COMP-3.           
020700     03  W-IDLOPNRM-X.                                                    
020800         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
020900     03  W-WDGXKEY-X.                                                     
021000         05  W-IDHTYP            PIC X(04)    VALUE '2215'.               
021100         05  W-LOW-VALUE         PIC X(26)    VALUE LOW-VALUE.            
021200*--------FYSISK NKL TILL INLA                                             
021300     03  W-W6D101KY-X.                                                    
021400         05  W-IDDC              PIC X(2)     VALUE '11'.                 
021500         05  W-IDLEVNR-D1        PIC X(5).                                
021600         05  W-IDFS              PIC X(8)     VALUE SPACE.                
021700         05  W-TIAVIDAT          PIC S9(7)    COMP-3.                     
021800     03  W-IDRADNR-INL-X.                                                 
021900         05  W-IDRADNR-INL      PIC S9(5)   COMP-3.                       
022000*--------FYSISK NKL TILL INLH (EG. INLI)                                  
022100     03  W-W6D1H1KY-MIN-X.                                                
022200         05  WH1-MIN-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
022300         05  WH1-MIN-IDDC        PIC X(2)     VALUE '11'.                 
022400         05  WH1-MIN-IDLEVNR     PIC X(5).                                
022500         05  WH1-MIN-IDFS        PIC X(8)                                 
022600                                              VALUE LOW-VALUE.            
022700         05  WH1-MIN-TIAVIDAT    PIC S9(7)    COMP-3                      
022800                                              VALUE ZERO.                 
022900         05  WH1-MIN-IDRADNR-INL PIC S9(5)    COMP-3                      
023000                                              VALUE ZERO.                 
023100                                                                          
023200     03  W-W6D1H1KY-MAX-X.                                                
023300         05  WH1-MAX-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
023400         05  WH1-MAX-IDDC        PIC X(2)     VALUE '11'.                 
023500         05  WH1-MAX-IDLEVNR     PIC X(5).                                
023600         05  WH1-MAX-IDFS        PIC X(8)                                 
023700                                              VALUE HIGH-VALUE.           
023800         05  WH1-MAX-TIAVIDAT    PIC S9(7)    COMP-3                      
023900                                              VALUE 9999999.              
024000         05  WH1-MAX-IDRADNR-INL PIC S9(5)    COMP-3                      
024100                                              VALUE 99999.                
024200     03  W-WDGXKEY-2231-X.                                                
024300         05  FILLER              PIC X(4)     VALUE '2231'.               
024400         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
024500     03  W-WDGXKEY-2232-X.                                                
024600         05  W-IDANSK-L          PIC S9(3)    VALUE ZERO COMP-3.          
024700         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
024800                                                                          
024900     SKIP2                                                                
025000*    --- STATUS-KOD FRÅN IMS                                              
025100 01  STATUS-WS                   PIC XX.                                  
025200     88  SEGMENT-FINNS                       VALUE '  '.                  
025300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
025500     SKIP2                                                                
025600 01  GODK-STATUSKODER.                                                    
025700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025800     SKIP3                                                                
025900 01  SSA1                        PIC X(128).                              
026000 01  SSA2                        PIC X(64).                               
026100 01  SSA3                        PIC X(64).                               
026200 01  SSA4                        PIC X(64).                               
026300     EJECT                                                                
026400*    --- IMS FUNKTIONSKODER                                               
026500*01  -COPY W0003                                                          
026600     EJECT                                                                
026700*    ---  DLI INPUT-OUTPUT AREA                                           
026800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD9'.                        
026900 01  DLI-IO-WDD9.                                                         
027000     03  IO-AREA    PIC X(100).                                           
027100     03  AREA-WDD901     REDEFINES IO-AREA.                               
027200*      05  -COPY WDD901                                                   
027300     SKIP3                                                                
027400     03  AREA-WDD902     REDEFINES IO-AREA.                               
027500*      05  -COPY WDD902                                                   
027600     EJECT                                                                
027700     03  AREA-WDD905     REDEFINES IO-AREA.                               
027800*      05 -COPY WDD905                                                    
027900     SKIP2                                                                
028000     03  AREA-WDD906     REDEFINES IO-AREA.                               
028100*      05  -COPY WDD906                                                   
028200     SKIP2                                                                
028300     03  AREA-WDD924     REDEFINES IO-AREA.                               
028400*      05  -COPY WDD924                                                   
028500     EJECT                                                                
028600*    ---  DLI INPUT-OUTPUT AREA                                           
028700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2215'.                    
028800 01  DLI-IO-WDGX2215.                                                     
028900*    03  -COPY WDGX01                                                     
029000     EJECT                                                                
029100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2216'.                    
029200 01  DLI-IO-WDGX2216.                                                     
029300*    03  -COPY WDGX2216                                                   
029400     EJECT                                                                
029500 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D111'.                      
029600 01  DLI-IO-W6D111.                                                       
029700*    03  -COPY W6D111                                                     
029800     EJECT                                                                
029900 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D1H1'.                      
030000 01  DLI-IO-W6D1H1.                                                       
030100*    03  -COPY W6D1H1                                                     
030200     EJECT                                                                
030300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
030400 01  DLI-IO-WDK601.                                                       
030500*    03  -COPY WDK601  -PRE ART-                                          
030600     EJECT                                                                
030700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
030800 01  DLI-IO-WDK611.                                                       
030900*    03  -COPY WDK611                                                     
031000     EJECT                                                                
031100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD906-B'.                    
031200 01  DLI-IO-WDD906-B.                                                     
031300*    03  -COPY WDD906   -PRE B-                                           
031400     EJECT                                                                
031500 01  DLI-IO-AREA-BX.                                                      
031600     03  IO-AREA-BX              PIC X(50)  VALUE SPACE.                  
031700     SKIP3                                                                
031800     03  WLXXBX01 REDEFINES IO-AREA-BX.                                   
031900*        05  -COPY WDGX01     -PRE XXBX-                                  
032000     SKIP3                                                                
032100     03  WLXXBX20 REDEFINES IO-AREA-BX.                                   
032200*        05  -COPY WDGX2232   -PRE XXBX-                                  
032300     EJECT                                                                
032400 01  DLI-IO-AREA-F1          PIC X(100).                                  
032500     SKIP2                                                                
032600*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
032700     EJECT                                                                
032800 LINKAGE SECTION.                                                         
032900                                                                          
033000                                                                          
033100*01  -COPY W0008  -PRE WDD9-                                              
033200     05  FILLER                  PIC X.                                   
033300                                                                          
033400*01  -COPY W0008  -PRE 2215-                                              
033500     05  FILLER                  PIC X.                                   
033600                                                                          
033700*01  -COPY W0008  -PRE W6D1-                                              
033800     05  FILLER                  PIC X.                                   
033900                                                                          
034000*01  -COPY W0008  -PRE W6D1H1-                                            
034100     05  FILLER                  PIC X.                                   
034200                                                                          
034300*01  -COPY W0008  -PRE WDK6-                                              
034400     05  FILLER                  PIC X.                                   
034500                                                                          
034600*01  -COPY W0008  -PRE WDD9B-                                             
034700     05  FILLER                  PIC X.                                   
034800     EJECT                                                                
034900*01  -COPY W0008  -PRE XXBX-                                              
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200*01  -COPY W0008  -PRE WDF1-                                              
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500 PROCEDURE DIVISION  USING WDD9-PCB 2215-PCB W6D1-PCB W6D1H1-PCB          
035600                           WDK6-PCB WDD9B-PCB XXBX-PCB WDF1-PCB.          
035700 MAIN SECTION.                                                            
035800     ENTRY 'DLITCBL' USING WDD9-PCB 2215-PCB W6D1-PCB W6D1H1-PCB          
035900                           WDK6-PCB WDD9B-PCB XXBX-PCB WDF1-PCB.          
036000                                                                          
036100                                                                          
036200     PERFORM A-INIT                                                       
036300                                                                          
036400     PERFORM IMS-GET-WDD9                                                 
036500     PERFORM UNTIL SEGMENT-SLUT                                           
036600       EVALUATE WDD9-SEG-NAME-FB                                          
036700         WHEN 'WDD901'                                                    
036800           PERFORM B-KOLL-WDK611                                          
036900         WHEN 'WDD902'                                                    
037000           IF CDC                                                         
037100              PERFORM C-KOLL-2216-AVIS                                    
037200           END-IF                                                         
037300         WHEN 'WDD905'                                                    
037400           IF CDC AND EMIL AND IDLEVNR-ALARM-OK                           
037500              PERFORM D-KOLL-WDD905                                       
037600           END-IF                                                         
037700         WHEN 'WDD906'                                                    
037800           IF CDC AND EMIL AND IDLEVNR-ALARM-OK                           
037900              PERFORM E-KOLL-WDD906                                       
038000           END-IF                                                         
038100         WHEN 'WDD924'                                                    
038200           IF CDC                                                         
038300              PERFORM F-KOLL-WDD924                                       
038400           END-IF                                                         
038500       END-EVALUATE                                                       
038600       PERFORM IMS-GET-WDD9                                               
038700     END-PERFORM                                                          
038800     PERFORM Z-FINIT                                                      
038900                                                                          
039000     MOVE ZERO TO RETURN-CODE                                             
039100     GOBACK                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 A-INIT SECTION.                                                          
039500                                                                          
039600     OPEN OUTPUT W22108                                                   
039700                 W22114                                                   
039800                                                                          
039900     ACCEPT W1-TIKLOCK FROM TIME                                          
040000     COMPUTE W1-TIKLOCK-9KOMPL = +999999999 - W1-TIKLOCK                  
040100                                                                          
040200     MOVE FUNCTION CURRENT-DATE (1:8) TO  W1-DAREGDAT                     
040300     COMPUTE W1-DAREGDAT-9KOMPL = 99999999 -                              
040400                                          W1-DAREGDAT                     
040500                                                                          
040600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
040700     MOVE D-AAR       TO DAGENS-DATUM-AAR    DAGENS-DATUM-AA              
040800* FIX                                                                     
040900*    MOVE 05          TO DAGENS-DATUM-AAR    DAGENS-DATUM-AA              
041000     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
041100* FIX                                                                     
041200*                                                                         
041300*    MOVE 11          TO DAGENS-DATUM-MAANAD                              
041400     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
041500* FIX                                                                     
041600*                                                                         
041700*    MOVE 07          TO DAGENS-DATUM-DAG                                 
041800     MOVE D-VECKA     TO DAGENS-DATUM-VV                                  
041900* FIX                                                                     
042000*                                                                         
042100*    MOVE 45          TO DAGENS-DATUM-VV                                  
042200     MOVE D-DAGNR     TO DAGENS-DATUM-D                                   
042300* FIX                                                                     
042400*                                                                         
042500*    MOVE 1           TO DAGENS-DATUM-D                                   
042600     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
042700                                                                          
042800*    010823  VI ANVÄNDER INTELÄNGRE DAG MINUS 1                           
042900*            UTAN ANVÄNDER KÖRNINGSDATUM FRÅN DATUMKORT                   
043000*    MOVE 3                  TO DAG-KDCALL                                
043100*    MOVE DAGENS-DATUM       TO DAG-TIAAMMDD-TOM                          
043200*    IF D-DAGNR = 1                                                       
043300*       MOVE +3              TO DAG-KVKALDAG                              
043400*    ELSE                                                                 
043500*       MOVE +2              TO DAG-KVKALDAG                              
043600*    END-IF                                                               
043700*                                                                         
043800*    CALL WDAGKONV USING DAG-KDCALL                                       
043900*              DAG-DATUM-AREA DAG-KDSVAR                                  
044000*                                                                         
044100*    IF DAG-KDSVAR = SPACE                                                
044200*      MOVE DAG-TIAAMMDD-FOM TO DAGENS-DATUM-MINUS-1                      
044300*      DISPLAY ' DAGENS - 1 '   DAGENS-DATUM-MINUS-1                      
044400*    ELSE                                                                 
044500*      MOVE 21 TO RKOD-ABEND                                              
044600*      PERFORM S99-ABEND                                                  
044700*    END-IF                                                               
044800                                                                          
044900     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
045000*    MOVE DAGENS-DATUM-MINUS-1 TO DAT-I-TIDATUM                           
045100     MOVE DAGENS-DATUM         TO DAT-I-TIDATUM                           
045200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
045300                     DAT-O-TIDATUM DAT-KDSVAR                             
045400     IF DAT-KDSVAR-OK                                                     
045500       MOVE DAT-TISEKEL      TO W-AAAAVV-SE                               
045600       MOVE DAT-TIAA-VECKA   TO W-AAAAVV-AA                               
045700       MOVE DAT-TIVV         TO W-AAAAVV-VV                               
045800       MOVE DAT-TID          TO W-TILEVDAG-SVAR                           
045900     ELSE                                                                 
046000       MOVE 22 TO RKOD-ABEND                                              
046100       PERFORM S99-ABEND                                                  
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046500 B-KOLL-WDK611      SECTION.                                              
046600                                                                          
046700     IF IDDC = WC-CDC-SE                                                  
046800        MOVE JA TO SW-CDC                                                 
046900        MOVE IDARTNR TO W-IDARTNR                                         
047000                         W-IDARTNR-D9                                     
047100        MOVE IDDC  TO W-IDDC-D9                                           
047200        PERFORM IMS-GET-WDK601                                            
047300        IF SEGMENT-FINNS                                                  
047400           PERFORM IMS-GNP-WDK611                                         
047500        ELSE                                                              
047600           MOVE ZERO TO ART-ART-KDPRODSL                                  
047700           MOVE ZERO TO CLAG-KDAVT                                        
047800           MOVE ZERO TO CLAG-IDANSK                                       
047900        END-IF                                                            
048000                                                                          
048100*****IF SEGMENT-FINNS AND (CLAG-ADLAGOMR = 14 OR 15 OR 16)                
048200*    IF SEGMENT-FINNS AND (CLAG-KDEFFMAN = 'E' OR 'B')                    
048300*       MOVE JA    TO SW-EMIL                                             
048400*    ELSE                                                                 
048500*       MOVE NEJ   TO SW-EMIL                                             
048600*    END-IF                                                               
048700     ELSE                                                                 
048800        MOVE NEJ TO SW-CDC                                                
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 C-KOLL-2216-AVIS   SECTION.                                              
049300                                                                          
049400     MOVE IDLEVNR     TO WS-IDLEVNR-EMIL                                  
049500                         WS-IDLEVNR-ALARM                                 
049600                                                                          
049700     IF EJ-GODK-EMIL-LEVNR                                                
049800        MOVE NEJ   TO SW-EMIL                                             
049900     ELSE                                                                 
050000        MOVE JA    TO SW-EMIL                                             
050100     END-IF                                                               
050200     IF NOT-GOOD-IDLEVNR                                                  
050300        MOVE NEJ   TO SW-IDLEVNR-ALARM                                    
050400     ELSE                                                                 
050500        MOVE JA    TO SW-IDLEVNR-ALARM                                    
050600     END-IF                                                               
050700     MOVE SPACE       TO SW-AVIS                                          
050800     IF EMIL                                                              
050900        MOVE IDLEVNR  TO W-IDLEVNR                                        
051000        PERFORM IMS-GET-WDGX2216                                          
051100        IF SEGMENT-FINNS AND 2216-FLAVIS = JA                             
051200           MOVE JA    TO SW-AVIS                                          
051300        END-IF                                                            
051400     END-IF                                                               
051500     MOVE KVBR        TO SPAR-KVBR                                        
051600     MOVE ZERO        TO SUM-KVAVROP-BR                                   
051700     MOVE NEJ         TO SW-WDF1                                          
051800     .                                                                    
051900     EJECT                                                                
052000 D-KOLL-WDD905      SECTION.                                              
052100                                                                          
052200     IF EMIL                                                              
052300        IF AVIS AND KDAVROP = 2                                           
052400*             LEV = LEV AND DAAVROP-AVS = KÖRNINGSDAG-1 AND               
052500*             KOD = 2                                                     
052600           MOVE W-AAAAVV-SVAR    TO W-DAAVROP                             
052700           MOVE W-TILEVDAG-SVAR  TO W-TILEVDAG                            
052800                                                                          
052900           IF SW-WDF1 = NEJ                                               
053000              PERFORM IMS-GET-WDF101                                      
053100              MOVE JA TO SW-WDF1                                          
053200           END-IF                                                         
053300           IF WDF1-FINNS AND F1-LEV-KVDAGAR-AVIAVV > ZERO                 
053400              PERFORM DC-JUSTERA-AVVIKELSE-AVI                            
053500           ELSE                                                           
053600             IF DAAVROP-AVS = W-DAAVROP AND                               
053700                TILEVDAG    = W-TILEVDAG                                  
053800                PERFORM DA-KOLL-W6D111                                    
053900             END-IF                                                       
054000           END-IF                                                         
054100        END-IF                                                            
054200                                                                          
054300        IF KDAVROP = 2                                                    
054400           IF SW-WDF1 = NEJ                                               
054500              PERFORM IMS-GET-WDF101                                      
054600              MOVE JA TO SW-WDF1                                          
054700           END-IF                                                         
054800           IF WDF1-FINNS AND F1-LEV-KVDAGAR-INLAVV > ZERO                 
054900              PERFORM DD-JUSTERA-AVVIKELSE-INL                            
055000           ELSE                                                           
055100              IF TIAVRDAT-INL = DAGENS-DATUM                              
055200                 PERFORM DB-KOLL-WDD906                                   
055300              END-IF                                                      
055400           END-IF                                                         
055500        END-IF                                                            
055600                                                                          
055700        MOVE WDD905                TO SPAR-WDD905                         
055800     END-IF                                                               
055900                                                                          
056000     IF KDAVROP = 2                                                       
056100        ADD KVAVROP                TO SUM-KVAVROP-BR                      
056200     END-IF                                                               
056300     .                                                                    
056400     EJECT                                                                
056500 DA-KOLL-W6D111     SECTION.                                              
056600                                                                          
056700                                                                          
056800     MOVE NEJ                 TO SW-IDLOPNRM                              
056900     MOVE ZERO                TO SUM-KVAVIS                               
057000*    FIXA NYCKLAR TILL W6D1-SEQH                                          
057100     MOVE W-IDARTNR           TO WH1-MIN-IDARTNR                          
057200                                 WH1-MAX-IDARTNR                          
057300     MOVE W-IDLEVNR           TO WH1-MIN-IDLEVNR                          
057400                                 WH1-MAX-IDLEVNR                          
057500     PERFORM IMS-GU-W6D1-SEQH                                             
057600     PERFORM UNTIL SEGMENT-SAKNAS                                         
057700*       FIXA NYCKLAR TILL W6D111                                          
057800        MOVE W-IDLEVNR        TO W-IDLEVNR-D1                             
057900        MOVE SEQH-IDFS        TO W-IDFS                                   
058000        MOVE SEQH-TIAVIDAT    TO W-TIAVIDAT                               
058100        MOVE SEQH-IDRADNR-INL TO W-IDRADNR-INL                            
058200        PERFORM IMS-GU-W6D111                                             
058300        IF SEGMENT-FINNS AND ART-IDLOPNRM = ZERO                          
058400           MOVE JA TO SW-IDLOPNRM                                         
058500           ADD ART-KVAVIS  TO  SUM-KVAVIS                                 
058600        END-IF                                                            
058700        PERFORM IMS-GN-W6D1-SEQH                                          
058800     END-PERFORM                                                          
058900                                                                          
059000     MOVE JA        TO SW-LARM                                            
059100     IF SUM-KVAVROP-BR > SPAR-KVBR                                        
059200        MOVE NEJ    TO SW-LARM                                            
059300        MOVE ART-ART-KDPRODSL    TO TEST-KDPRODSL                         
059400        IF (KDPRODSL-BYTES AND CLAG-KDAVT = ZERO) OR                      
059500           W-IDLEVNR = '1002 '                                            
059600           MOVE JA  TO SW-LARM                                            
059700        END-IF                                                            
059800     END-IF                                                               
059900                                                                          
060000     IF ((SW-IDLOPNRM = NEJ) OR                                           
060100        (SUM-KVAVIS        <  KVAVROP))                                   
060200        AND SW-LARM = JA                                                  
060300*       SKAPA LARM 220                                                    
060400        MOVE W1-DAREGDAT-9KOMPL TO UT-LAK-DAREGDAT-9KOMPL                 
060500        MOVE W-IDARTNR          TO UT-LAK-IDARTNR                         
060600        MOVE WC-CDC-SE          TO UT-LAK-IDDC                            
060700        MOVE CLAG-IDANSK        TO UT-LAK-IDANSK                          
060800                                   W-IDANSK-L                             
060900        PERFORM IMS-GET-XXBX-2232                                         
061000        IF SEGMENT-FINNS                                                  
061100          MOVE XXBX-2232-IDANSK-LARM                                      
061200                                TO UT-LAK-IDANSK                          
061300        END-IF                                                            
061400        MOVE W-IDLEVNR          TO UT-LAK-IDLEVNR                         
061500        MOVE 220                TO UT-LAK-KDLARM                          
061600        MOVE '*'                TO UT-LAK-FLNYLARM                        
061700        MOVE SUM-KVAVIS         TO UT-LAK-KVAVIS                          
061800        MOVE KVAVROP            TO UT-LAK-KVAVROP                         
061900        MOVE ZERO               TO UT-LAK-TIAAMMDD-AVS                    
062000                                                                          
062100        MOVE 'AAVVD'            TO DAT-KDDATFORM                          
062200        MOVE DAAVROP-AVS        TO WS-AAAAVV                              
062300        MOVE TILEVDAG           TO WS-TILEVDAG                            
062400        MOVE WS-AAVVD           TO DAT-I-TIDATUM                          
062500        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
062600                            DAT-O-TIDATUM DAT-KDSVAR                      
062700        IF DAT-KDSVAR-OK                                                  
062800           MOVE DAT-TIAAMMDD    TO UT-LAK-TIAAMMDD                        
062900        ELSE                                                              
063000           MOVE 23 TO RKOD-ABEND                                          
063100           PERFORM S99-ABEND                                              
063200        END-IF                                                            
063300                                                                          
063400        PERFORM S11-SKRIV-W22108                                          
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800 DB-KOLL-WDD906     SECTION.                                              
063900                                                                          
064000     MOVE ZERO                TO SUM-KVAVROP-AVB                          
064100     MOVE NEJ                 TO SW-WDD906-SAKNAS                         
064200*    FIXA NYCKLAR TILL WDD906                                             
064300     MOVE DAAVROP-AVS         TO W-DAAVROP   WS-AAAAVV                    
064400     MOVE TILEVDAG            TO W-TILEVDAG  WS-TILEVDAG                  
064500     PERFORM IMS-GU-WDD906B                                               
064600     IF SEGMENT-SAKNAS                                                    
064700        MOVE JA TO SW-WDD906-SAKNAS                                       
064800     END-IF                                                               
064900     PERFORM UNTIL SEGMENT-SAKNAS                                         
065000        ADD B-KVAVROP-AVB     TO SUM-KVAVROP-AVB                          
065100        PERFORM IMS-GN-WDD906B                                            
065200     END-PERFORM                                                          
065300                                                                          
065400     MOVE JA        TO SW-LARM                                            
065500     IF SUM-KVAVROP-BR > SPAR-KVBR                                        
065600        MOVE NEJ    TO SW-LARM                                            
065700        MOVE ART-ART-KDPRODSL    TO TEST-KDPRODSL                         
065800        IF (KDPRODSL-BYTES AND CLAG-KDAVT = ZERO) OR                      
065900           W-IDLEVNR = '1002 '                                            
066000           MOVE JA  TO SW-LARM                                            
066100        END-IF                                                            
066200     END-IF                                                               
066300                                                                          
066400     IF ((SW-WDD906-SAKNAS = JA) OR                                       
066500        ( KVAVROP NOT = ZERO))                                            
066600        AND SW-LARM = JA                                                  
066700********                       (KVAVROP + SUM-KVAVROP-AVB))               
066800*       SKAPA LARM 225                                                    
066900        MOVE W1-DAREGDAT-9KOMPL TO UT-LAK-DAREGDAT-9KOMPL                 
067000        MOVE W-IDARTNR          TO UT-LAK-IDARTNR                         
067100        MOVE WC-CDC-SE          TO UT-LAK-IDDC                            
067200        MOVE CLAG-IDANSK        TO UT-LAK-IDANSK                          
067300                                   W-IDANSK-L                             
067400        PERFORM IMS-GET-XXBX-2232                                         
067500        IF SEGMENT-FINNS                                                  
067600          MOVE XXBX-2232-IDANSK-LARM                                      
067700                                TO UT-LAK-IDANSK                          
067800        END-IF                                                            
067900        MOVE W-IDLEVNR          TO UT-LAK-IDLEVNR                         
068000        MOVE 225                TO UT-LAK-KDLARM                          
068100        MOVE '*'                TO UT-LAK-FLNYLARM                        
068200        MOVE SUM-KVAVROP-AVB    TO UT-LAK-KVAVIS                          
068300        MOVE KVAVROP            TO UT-LAK-KVAVROP                         
068400        ADD  SUM-KVAVROP-AVB    TO UT-LAK-KVAVROP                         
068500        MOVE TIAVRDAT-INL       TO UT-LAK-TIAAMMDD                        
068600                                                                          
068700        MOVE 'AAVVD'            TO DAT-KDDATFORM                          
068800        MOVE WS-AAVVD           TO DAT-I-TIDATUM                          
068900        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
069000                            DAT-O-TIDATUM DAT-KDSVAR                      
069100        IF DAT-KDSVAR-OK                                                  
069200           MOVE DAT-TIAAMMDD    TO UT-LAK-TIAAMMDD-AVS                    
069300        ELSE                                                              
069400           MOVE 26 TO RKOD-ABEND                                          
069500           PERFORM S99-ABEND                                              
069600        END-IF                                                            
069700                                                                          
069800        PERFORM S11-SKRIV-W22108                                          
069900     END-IF                                                               
070000     .                                                                    
070100     EJECT                                                                
070200 DC-JUSTERA-AVVIKELSE-AVI SECTION.                                        
070300                                                                          
070400*  JUSTERING GÖRS MED ANTALET DAGAR (X) I KVDAGAR-AVIAVV                  
070500*  DVS ATT VI FÅR JÄMFÖRA MED DAAVROP-AVS  + X                            
070600                                                                          
070700     MOVE DAAVROP-AVS        TO W-JUST-DAAVROP                            
070800     MOVE TILEVDAG           TO W-JUST-TILEVDAG                           
070900     MOVE W-JUST-AAVVD       TO DAT-I-TIDATUM                             
071000     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
071100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
071200                     DAT-O-TIDATUM DAT-KDSVAR                             
071300     IF DAT-KDSVAR-OK                                                     
071400       CONTINUE                                                           
071500     ELSE                                                                 
071600       MOVE 28 TO RKOD-ABEND                                              
071700       PERFORM S99-ABEND                                                  
071800     END-IF                                                               
071900                                                                          
072000     IF DAT-TIAAMMDD > 500000 OR                                          
072100        DAT-TIAAMMDD < 060101                                             
072200        MOVE 060101  TO DAT-TIAAMMDD                                      
072300     END-IF                                                               
072400                                                                          
072500     MOVE 2                  TO WORK-KDCALL                               
072600     MOVE '11'               TO WORK-IDDC                                 
072700     MOVE DAT-TIAAMMDD       TO WORK-TIAAMMDD-FOM                         
072800     MOVE F1-LEV-KVDAGAR-AVIAVV  TO WORK-KVWORKD                          
072900     ADD  +1                 TO WORK-KVWORKD                              
073000     CALL WORKDAY USING WORK-KDCALL                                       
073100               WORK-DATE-AREA WORK-KDSVAR                                 
073200     IF WORK-KDSVAR-OK                                                    
073300       CONTINUE                                                           
073400     ELSE                                                                 
073500       MOVE 27 TO RKOD-ABEND                                              
073600       PERFORM S99-ABEND                                                  
073700     END-IF                                                               
073800                                                                          
073900     IF WORK-TIAAMMDD-TOM = DAGENS-DATUM                                  
074000        PERFORM DA-KOLL-W6D111                                            
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400 DD-JUSTERA-AVVIKELSE-INL SECTION.                                        
074500                                                                          
074600*  JUSTERING GÖRS MED ANTALET DAGAR (X) I KVDAGAR-INLAVV                  
074700*  DVS ATT VI FÅR JÄMFÖRA MED TIAVRDAT-INL + X                            
074800                                                                          
074900*    FIX FÖR ATT WORKDAY INTE KLARAR GAMLA DATUM                          
075000     IF TIAVRDAT-INL > 500000 OR TIAVRDAT-INL < 060101                    
075100        MOVE 060101 TO TIAVRDAT-INL                                       
075200     END-IF                                                               
075300*                                                                         
075400     MOVE 2                  TO WORK-KDCALL                               
075500     MOVE '11'               TO WORK-IDDC                                 
075600     MOVE TIAVRDAT-INL       TO WORK-TIAAMMDD-FOM                         
075700     MOVE F1-LEV-KVDAGAR-INLAVV  TO WORK-KVWORKD                          
075800     ADD  +1                 TO WORK-KVWORKD                              
075900     CALL WORKDAY USING WORK-KDCALL                                       
076000               WORK-DATE-AREA WORK-KDSVAR                                 
076100     IF WORK-KDSVAR-OK                                                    
076200       CONTINUE                                                           
076300     ELSE                                                                 
076400       MOVE 29 TO RKOD-ABEND                                              
076500       PERFORM S99-ABEND                                                  
076600     END-IF                                                               
076700                                                                          
076800     IF WORK-TIAAMMDD-TOM = DAGENS-DATUM                                  
076900        PERFORM DB-KOLL-WDD906                                            
077000     END-IF                                                               
077100     .                                                                    
077200     EJECT                                                                
077300 E-KOLL-WDD906      SECTION.                                              
077400                                                                          
077500     IF EMIL AND (SPAR-KDAVROP = 9 OR 2)                                  
077600        MOVE IDLOPNRM-PL  TO WS-IDLOPNRM-PL                               
077700        IF WS-IDLOPNRM-AAVVD = DAGENS-DATUM-AAVVD                         
077800*          TEST DAGENS-DATUM < (SPAR-TIAVRDAT-INL - 4)                    
077900           MOVE 3                  TO WORK-KDCALL                         
078000           MOVE '11'               TO WORK-IDDC                           
078100* TEST FIX                                                                
078200           IF SPAR-TIAVRDAT-INL > 950101                                  
078300              MOVE 060101 TO SPAR-TIAVRDAT-INL                            
078400           END-IF                                                         
078500           IF SPAR-TIAVRDAT-INL < 060101                                  
078600              MOVE 060101 TO SPAR-TIAVRDAT-INL                            
078700           END-IF                                                         
078800* TEST FIX                                                                
078900           MOVE SPAR-TIAVRDAT-INL  TO WORK-TIAAMMDD-TOM                   
079000           MOVE +5                 TO WORK-KVWORKD                        
079100           CALL WORKDAY USING WORK-KDCALL                                 
079200                     WORK-DATE-AREA WORK-KDSVAR                           
079300           IF WORK-KDSVAR-OK                                              
079400             CONTINUE                                                     
079500           ELSE                                                           
079600             MOVE 25 TO RKOD-ABEND                                        
079700             PERFORM S99-ABEND                                            
079800           END-IF                                                         
079900                                                                          
080000           IF DAGENS-DATUM < WORK-TIAAMMDD-FOM                            
080100              PERFORM EA-LARM-TIDIGT                                      
080200           END-IF                                                         
080300        END-IF                                                            
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700 EA-LARM-TIDIGT SECTION.                                                  
080800                                                                          
080900*       SKAPA LARM 230                                                    
081000        MOVE W1-DAREGDAT-9KOMPL TO UT-LAK-DAREGDAT-9KOMPL                 
081100        MOVE W-IDARTNR          TO UT-LAK-IDARTNR                         
081200        MOVE WC-CDC-SE          TO UT-LAK-IDDC                            
081300        MOVE CLAG-IDANSK        TO UT-LAK-IDANSK                          
081400                                   W-IDANSK-L                             
081500        PERFORM IMS-GET-XXBX-2232                                         
081600        IF SEGMENT-FINNS                                                  
081700          MOVE XXBX-2232-IDANSK-LARM                                      
081800                                TO UT-LAK-IDANSK                          
081900        END-IF                                                            
082000        MOVE W-IDLEVNR          TO UT-LAK-IDLEVNR                         
082100        MOVE 230                TO UT-LAK-KDLARM                          
082200        MOVE '*'                TO UT-LAK-FLNYLARM                        
082300        MOVE KVAVROP-AVB        TO UT-LAK-KVAVIS                          
082400        MOVE SPAR-TIAVRDAT-INL  TO UT-LAK-TIAAMMDD                        
082500        MOVE ZERO               TO UT-LAK-TIAAMMDD-AVS                    
082600        MOVE SPAR-KVAVROP       TO UT-LAK-KVAVROP                         
082700        ADD  KVAVROP-AVB        TO UT-LAK-KVAVROP                         
082800***     (ELLER SKALL SAMTLIGA 06-SEGMS AVB ADDERAS ?)                     
082900                                                                          
083000        PERFORM S11-SKRIV-W22108                                          
083100     .                                                                    
083200     EJECT                                                                
083300 F-KOLL-WDD924      SECTION.                                              
083400                                                                          
083500     IF LEV-KVAVIS-BSKKVAR > 0                                            
083600     MOVE 2                     TO WORK-KDCALL                            
083700     MOVE '11'                  TO WORK-IDDC                              
083800     MOVE LEV-TILEVBSK-INL      TO WORK-TIAAMMDD-FOM                      
083900     MOVE +3                    TO WORK-KVWORKD                           
084000     CALL WORKDAY USING WORK-KDCALL                                       
084100               WORK-DATE-AREA WORK-KDSVAR                                 
084200     IF WORK-KDSVAR-OK                                                    
084300       CONTINUE                                                           
084400     ELSE                                                                 
084500       MOVE 24 TO RKOD-ABEND                                              
084600       PERFORM S99-ABEND                                                  
084700     END-IF                                                               
084800                                                                          
084900     IF DAGENS-DATUM = WORK-TIAAMMDD-TOM                                  
085000                                                                          
085100*       SKAPA LARM 221                                                    
085200        MOVE CLAG-IDANSK        TO UT2-IDANSK                             
085300        MOVE W-IDARTNR          TO UT2-IDARTNR                            
085400        MOVE 221                TO UT2-KDLARM                             
085500        MOVE WS-IDLEVNR-EMIL    TO UT2-IDLEVNR                            
085600                                                                          
085700        PERFORM S12-SKRIV-W22114                                          
085800     END-IF                                                               
085900     END-IF                                                               
086000     .                                                                    
086100     EJECT                                                                
086200 Z-FINIT SECTION.                                                         
086300                                                                          
086400     CLOSE W22108                                                         
086500           W22114                                                         
086600     SKIP2                                                                
086700     MOVE 'S' TO POSTSUM-OPKOD                                            
086800     CALL POSTSUM USING POSTSUM-PARM                                      
086900     .                                                                    
087000     EJECT                                                                
087100 S11-SKRIV-W22108 SECTION.                                                
087200                                                                          
087300     COMPUTE W1-TIKLOCK-9KOMPL = W1-TIKLOCK-9KOMPL - 1                    
087400     MOVE W1-TIKLOCK-9KOMPL  TO UT-LAK-TIKLOCK-9KOMPL                     
087500                                                                          
087600     WRITE UT-POST         FROM UT-AREA                                   
087700                                                                          
087800     MOVE 'LARM'             TO POSTSUM-TRANSTYP                          
087900     MOVE 'W22108'           TO POSTSUM-FDNAMN                            
088000     MOVE 'W22107D1'         TO POSTSUM-DDNAMN2                           
088100     CALL POSTSUM         USING POSTSUM-PARM                              
088200     .                                                                    
088300     EJECT                                                                
088400 S12-SKRIV-W22114 SECTION.                                                
088500                                                                          
088600     WRITE UT2-POST FROM UT2-AREA                                         
088700                                                                          
088800     MOVE 'LARM'     TO POSTSUM-TRANSTYP                                  
088900     MOVE 'W22114'   TO POSTSUM-FDNAMN                                    
089000     MOVE 'W22107D2' TO POSTSUM-DDNAMN2                                   
089100     CALL POSTSUM USING POSTSUM-PARM                                      
089200     .                                                                    
089300     EJECT                                                                
089400 S99-ABEND SECTION.                                                       
089500                                                                          
089600     SKIP2                                                                
089700     MOVE 'S' TO POSTSUM-OPKOD                                            
089800     CALL POSTSUM USING POSTSUM-PARM                                      
089900     CALL ABEND USING RKOD-ABEND                                          
090000     .                                                                    
090100     EJECT                                                                
090200* --- IMS SEKTIONER ---                                                   
090300                                                                          
090400                                                                          
090500 IMS-GET-WDD9   SECTION.                                                  
090600                                                                          
090700     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-WDD9                           
090800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
090900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
091000     PERFORM IMS-STATUSKONTROLL                                           
091100     .                                                                    
091200     EJECT                                                                
091300 IMS-GET-WDGX2215 SECTION.                                                
091400                                                                          
091500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
091600          DELIMITED BY SIZE INTO SSA1                                     
091700     MOVE '  GE' TO GODK-STATUSKODER                                      
091800     CALL CBLTDLI USING GNP 2215-PCB DLI-IO-WDGX2215 SSA1                 
091900     MOVE 2215-STATUS-CODE TO STATUS-WS                                   
092000     PERFORM IMS-STATUSKONTROLL                                           
092100     .                                                                    
092200     EJECT                                                                
092300 IMS-GET-WDGX2216 SECTION.                                                
092400                                                                          
092500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
092600          DELIMITED BY SIZE INTO SSA1                                     
092700     STRING 'WDR242  (IDLEVNR  =' W-IDLEVNR-X ')'                         
092800          DELIMITED BY SIZE INTO SSA2                                     
092900     MOVE '  GE' TO GODK-STATUSKODER                                      
093000     CALL CBLTDLI USING GU 2215-PCB DLI-IO-WDGX2216 SSA1 SSA2             
093100     MOVE 2215-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     .                                                                    
093400     EJECT                                                                
093500 IMS-GU-WDD906B SECTION.                                                  
093600                                                                          
093700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
093800          DELIMITED BY SIZE INTO SSA1                                     
093900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
094000          DELIMITED BY SIZE INTO SSA2                                     
094100     STRING 'WDD905  (DAAVROP  =' W-DAAVROP-X ')'                         
094200          DELIMITED BY SIZE INTO SSA3                                     
094300     STRING 'WDD906   '                                                   
094400          DELIMITED BY SIZE INTO SSA4                                     
094500     MOVE '  GE' TO GODK-STATUSKODER                                      
094600     CALL CBLTDLI USING GU WDD9B-PCB DLI-IO-WDD906-B SSA1                 
094700                                                     SSA2                 
094800                                                     SSA3                 
094900                                                     SSA4                 
095000     MOVE WDD9B-STATUS-CODE TO STATUS-WS                                  
095100     PERFORM IMS-STATUSKONTROLL                                           
095200     .                                                                    
095300     EJECT                                                                
095400 IMS-GN-WDD906B SECTION.                                                  
095500                                                                          
095600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
095700          DELIMITED BY SIZE INTO SSA1                                     
095800     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
095900          DELIMITED BY SIZE INTO SSA2                                     
096000     STRING 'WDD905  (DAAVROP  =' W-DAAVROP-X ')'                         
096100          DELIMITED BY SIZE INTO SSA3                                     
096200     STRING 'WDD906   '                                                   
096300          DELIMITED BY SIZE INTO SSA4                                     
096400     MOVE '  GE' TO GODK-STATUSKODER                                      
096500     CALL CBLTDLI USING GN WDD9B-PCB DLI-IO-WDD906-B SSA1                 
096600                                                     SSA2                 
096700                                                     SSA3                 
096800                                                     SSA4                 
096900     MOVE WDD9B-STATUS-CODE TO STATUS-WS                                  
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200     EJECT                                                                
097300 IMS-GET-WDK601 SECTION.                                                  
097400                                                                          
097500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
097600          DELIMITED BY SIZE INTO SSA1                                     
097700     MOVE '  GE' TO GODK-STATUSKODER                                      
097800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
097900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
098000     PERFORM IMS-STATUSKONTROLL                                           
098100     .                                                                    
098200     SKIP3                                                                
098300 IMS-GNP-WDK611 SECTION.                                                  
098400                                                                          
098500     STRING 'WDK611    '                                                  
098600          DELIMITED BY SIZE INTO SSA1                                     
098700     MOVE '  GE' TO GODK-STATUSKODER                                      
098800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
098900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
099000     PERFORM IMS-STATUSKONTROLL                                           
099100     .                                                                    
099200     EJECT                                                                
099300******************************************************************        
099400*    W6D1-PCB                                                             
099500******************************************************************        
099600     SKIP3                                                                
099700*----------------------------------------------------------------*        
099800 IMS-GU-W6D111    SECTION.                                                
099900     STRING 'W6D101  (W6D101KY =' W-W6D101KY-X ')'                        
100000             DELIMITED BY SIZE INTO SSA1                                  
100100     STRING 'W6D111  (IDRADNRI =' W-IDRADNR-INL-X ')'                     
100200          DELIMITED BY SIZE INTO SSA2                                     
100300     MOVE '  GE'            TO GODK-STATUSKODER                           
100400     CALL CBLTDLI USING GU  W6D1-PCB                                      
100500                            DLI-IO-W6D111                                 
100600                            SSA1                                          
100700                            SSA2                                          
100800     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
100900     PERFORM IMS-STATUSKONTROLL                                           
101000     .                                                                    
101100     EJECT                                                                
101200******************************************************************        
101300*    W6D1H1-PCB                                                           
101400******************************************************************        
101500     SKIP3                                                                
101600*----------------------------------------------------------------*        
101700 IMS-GU-W6D1-SEQH     SECTION.                                            
101800     STRING 'W6D1H1  (W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
101900                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
102000             DELIMITED BY SIZE INTO SSA1                                  
102100     MOVE '  GE'                 TO GODK-STATUSKODER                      
102200     CALL CBLTDLI USING GU       W6D1H1-PCB                               
102300                                 DLI-IO-W6D1H1                            
102400                                 SSA1                                     
102500     MOVE W6D1H1-STATUS-CODE     TO STATUS-WS                             
102600     PERFORM IMS-STATUSKONTROLL                                           
102700     .                                                                    
102800     EJECT                                                                
102900     SKIP3                                                                
103000*----------------------------------------------------------------*        
103100 IMS-GN-W6D1-SEQH     SECTION.                                            
103200     STRING 'W6D1H1  (W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
103300                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
103400             DELIMITED BY SIZE INTO SSA1                                  
103500     MOVE '  GE'                 TO GODK-STATUSKODER                      
103600     CALL CBLTDLI USING GN       W6D1H1-PCB                               
103700                                 DLI-IO-W6D1H1                            
103800                                 SSA1                                     
103900     MOVE W6D1H1-STATUS-CODE     TO STATUS-WS                             
104000     PERFORM IMS-STATUSKONTROLL                                           
104100     .                                                                    
104200     EJECT                                                                
104300 IMS-GET-XXBX-2231 SECTION.                                               
104400                                                                          
104500     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
104600          DELIMITED BY SIZE INTO SSA1                                     
104700     MOVE '  ' TO GODK-STATUSKODER                                        
104800     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA-BX SSA1                   
104900     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
105000     PERFORM IMS-STATUSKONTROLL                                           
105100     .                                                                    
105200                                                                          
105300                                                                          
105400 IMS-GET-XXBX-2232 SECTION.                                               
105500                                                                          
105600     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
105700          DELIMITED BY SIZE INTO SSA1                                     
105800     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
105900          DELIMITED BY SIZE INTO SSA2                                     
106000     MOVE '  GE' TO GODK-STATUSKODER                                      
106100     CALL CBLTDLI USING GU  XXBX-PCB DLI-IO-AREA-BX SSA1 SSA2             
106200     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
106300     PERFORM IMS-STATUSKONTROLL                                           
106400     .                                                                    
106500     EJECT                                                                
106600 IMS-GET-WDF101 SECTION.                                                  
106700                                                                          
106800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
106900          DELIMITED BY SIZE INTO SSA1                                     
107000     MOVE '  GE' TO GODK-STATUSKODER                                      
107100     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
107200     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     IF SEGMENT-FINNS                                                     
107500        MOVE JA  TO SW-WDF1-FINNS                                         
107600     ELSE                                                                 
107700        MOVE NEJ TO SW-WDF1-FINNS                                         
107800     END-IF                                                               
107900     .                                                                    
108000     EJECT                                                                
108100 IMS-STATUSKONTROLL SECTION.                                              
108200                                                                          
108300     SET STATUS-IX TO 1                                                   
108400     SEARCH GODK-STATUS                                                   
108500       AT END                                                             
108600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
108700           DELIMITED BY SIZE INTO FELTEXT                                 
108800         DISPLAY FELTEXT                                                  
108900         CALL FELLOG                                                      
109000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
109100         CONTINUE                                                         
109200     END-SEARCH                                                           
109300     .                                                                    
