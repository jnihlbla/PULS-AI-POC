000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2219A00.                                                
000300 AUTHOR.         G KJELLSON                                               
000400 DATE-WRITTEN.   AUG 2012                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER AVROP MED DATUM FÖREGÅENDE ARBETSDAG (SB),                 
000900*        GÖR DIVERSE KONTROLLER OCH VID BEHOV SKAPAR                      
001000*        LARMUNDERLAG (POSTER TILL W2219A)                                
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDD9   SB                                  
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDR2 (WDGX2206) HTYP 2205                  
001500*        PROGRAMMET LÄSER      W6D1                                       
001600*        PROGRAMMET LÄSER      WDB6                                       
001601*        PROGRAMMET LÄSER      WDF1                                       
001602*        PROGRAMMET LÄSER      WDK6                                       
001610*        PROGRAMMET LÄSER      WDK7                                       
001700*        PROGRAMMET LÄSER      WDD9                                       
001800*        PROGRAMMET LÄSER      WLXXBX (WDR2) HTYP 2231                    
001900*                                                                         
002000*        PROGRAMMET ÄR KOPIERAT FRÅN W2210700 OCH ANPASSAT FÖR            
002100*        KINA                                                             
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*          --- LARMPOSTER TILL W22108-PGM                                 
003600     SELECT W2219A                     ASSIGN TO W2219AD1.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W2219A                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY WDD401   -PRE  UT-  -L.                                   
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W2219A00'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005401 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
005402 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
005406 77  F16-NDC-KVDAGAR-AVIAVV      PIC S9(3)   VALUE ZERO COMP-3.           
005407 77  F16-NDC-KVDAGAR-INLAVV      PIC S9(3)   VALUE ZERO COMP-3.           
005410 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
005500 01  SW-KINA-USA-ARTIKEL         PIC X       VALUE SPACE.                 
005600     88  KINA-USA-ARTIKEL                    VALUE 'J'.                   
005700 01  SW-AVIS                     PIC X       VALUE SPACE.                 
005800     88  AVIS                                VALUE 'J'.                   
005900 01  SW-WDF1                     PIC X       VALUE SPACE.                 
006000     88  WDF1-LAEST                          VALUE 'J'.                   
006100 01  SW-WDF1-FINNS               PIC X       VALUE SPACE.                 
006200     88  WDF1-FINNS                          VALUE 'J'.                   
006300 01  SW-IDLOPNRM                 PIC X       VALUE SPACE.                 
006500 01  SW-WDD906-SAKNAS            PIC X       VALUE SPACE.                 
006600 01  W1-DAREGDAT                 PIC 9(08).                               
006700 01  W1-DAREGDAT-9KOMPL          PIC 9(08).                               
006800 77  W1-TIKLOCK                  PIC 9(09).                               
006900 77  W1-TIKLOCK-9KOMPL           PIC 9(09)   COMP-3.                      
007000 01  SUM-KVAVIS                  PIC S9(7)   COMP-3.                      
007100 01  SUM-KVAVROP-AVB             PIC S9(7)   COMP-3.                      
007400     EJECT                                                                
007500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008000 01  DAGENS-DATUM-AAVVD          PIC 9(5)    VALUE ZERO.                  
008100 01  FILLER REDEFINES DAGENS-DATUM-AAVVD.                                 
008200     03  DAGENS-DATUM-AA         PIC 9(2).                                
008300     03  DAGENS-DATUM-VV         PIC 9(2).                                
008400     03  DAGENS-DATUM-D          PIC 9(1).                                
008500 01  W-AAMMDD-SVAR               PIC 9(6)    VALUE ZERO.                  
008600 01  W-AAAAVV-SVAR               PIC 9(6)    VALUE ZERO.                  
008700 01  FILLER REDEFINES W-AAAAVV-SVAR.                                      
008800     03  W-AAAAVV-SE             PIC 9(2).                                
008900     03  W-AAAAVV-AA             PIC 9(2).                                
009000     03  W-AAAAVV-VV             PIC 9(2).                                
009100 01  W-TILEVDAG-SVAR             PIC 9(1)    VALUE ZERO.                  
009200 01  DAGENS-DATUM-MINUS-1        PIC 9(6)    VALUE ZERO.                  
009300 01  W-JUST-AAAAVVD              PIC 9(7)    VALUE ZERO.                  
009400 01  FILLER REDEFINES W-JUST-AAAAVVD.                                     
009500     03  W-JUST-DAAVROP          PIC 9(6).                                
009600     03  W-JUST-TILEVDAG         PIC 9(1).                                
009700 01  FILLER REDEFINES W-JUST-AAAAVVD.                                     
009800     03  FILLER                  PIC 9(2).                                
009900     03  W-JUST-AAVVD            PIC 9(5).                                
010000                                                                          
010100 01  WS-AAAAVVD                  PIC 9(7).                                
010200 01  FILLER   REDEFINES WS-AAAAVVD.                                       
010300     03  FILLER                  PIC 9(2).                                
010400     03  WS-AAVV                 PIC 9(4).                                
010500     03  WS-TILEVDAG             PIC 9(1).                                
010600 01  FILLER   REDEFINES WS-AAAAVVD.                                       
010700     03  WS-AAAAVV               PIC 9(6).                                
010800     03  FILLER                  PIC 9(1).                                
010900 01  FILLER   REDEFINES WS-AAAAVVD.                                       
011000     03  FILLER                  PIC 9(2).                                
011100     03  WS-AAVVD                PIC 9(5).                                
011200 01  WS-IDLOPNRM-PL              PIC 9(9).                                
011300 01  FILLER   REDEFINES WS-IDLOPNRM-PL.                                   
011400     03  WS-IDLOPNRM-AAVVD       PIC 9(5).                                
011500     03  FILLER                  PIC 9(4).                                
011600                                                                          
011700 01  SPAR-AREA.                                                           
011800*    03 -COPY WDD905    -PRE  SPAR-                                       
011900     EJECT                                                                
012000 01  DYNAMISKA-SUBPROGRAM.                                                
012100*                                                                         
012200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
012700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012900     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
013000     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
013100     SKIP2                                                                
013200*    --- PARAMETRAR TILL ABEND                                            
013300                                                                          
013400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013700     SKIP2                                                                
013800 01  FELTEXT.                                                             
013900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014100     EJECT                                                                
014200*01  -COPY WWDCKONS                                                       
014300     EJECT                                                                
014400*01  -COPY WDAGAREA                                                       
014500     EJECT                                                                
014600*01  -COPY WORKAREA                                                       
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL DATKORT                                          
014900*                                                                         
015000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W2219A'.              
015100     SKIP2                                                                
015200 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
015300     SKIP2                                                                
015400*01  -COPY WDATKORT                                                       
015500     EJECT                                                                
015600*    --- PARAMETRAR TILL POSTSUM                                          
015700*                                                                         
015800*01  -COPY W0005   -PRE  POSTSUM-                                         
015900     EJECT                                                                
016000*01  -COPY WDATAREA                                                       
016100     EJECT                                                                
016200*    -COPY W200EMAB                                                       
016300     EJECT                                                                
016400 01  UT-AREA-START               PIC X(24)   VALUE                        
016500                                 'UT-AREA-START  '.                       
016600     SKIP2                                                                
016700                                                                          
016800*01  AREA -COPY WDD401       -PRE UT-                                     
016900     EJECT                                                                
017000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017100*                                                                         
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400 01  NYCKLAR-TILL-DLI.                                                    
017500     03  W-IDARTNR-X.                                                     
017600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017700     03  W-IDDC-X.                                                        
017800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017900     03  W-IDDC-B6-X.                                                     
017910         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
017920     03  W-IDDC-F16-X.                                                    
017930         05  W-IDDC-F16          PIC X(2)    VALUE SPACE.                 
018000     03  W-KDSEGKEY-X.                                                    
018100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
018200     03  W-IDLEVNR-X.                                                     
018300         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
018400     03  W-WDD901KY-X.                                                    
018500         07  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
018600         07  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
018700     03  W-WDD905KY-X.                                                    
018800         05  W-DAAVROP-X.                                                 
018900             07  W-DAAVROP       PIC 9(6)    VALUE ZERO.                  
019000         05  W-TILEVDAG-X.                                                
019100             07  W-TILEVDAG      PIC S9(1)   VALUE ZERO COMP-3.           
019200     03  W-IDLOPNRM-X.                                                    
019300         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
019310                                                                          
019400     03  W-WDGXKEY-2205-X.                                                
019500         05  W-IDHTYP            PIC X(04)    VALUE '2205'.               
019600         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
019610     03  W-KY2206-X.                                                      
019620         05  W-IDLEVNR-2206      PIC X(05)    VALUE SPACE.                
019630         05  W-IDDC-2206         PIC X(02)    VALUE SPACE.                
019640                                                                          
019700*--------FYSISK NKL TILL INLA                                             
019800     03  W-W6D101KY-X.                                                    
019900         05  W-IDDC-D1           PIC X(2)     VALUE SPACE.                
020000         05  W-IDLEVNR-D1        PIC X(5).                                
020100         05  W-IDFS              PIC X(8)     VALUE SPACE.                
020200         05  W-TIAVIDAT          PIC S9(7)    COMP-3.                     
020300     03  W-IDRADNR-INL-X.                                                 
020400         05  W-IDRADNR-INL      PIC S9(5)   COMP-3.                       
020500*--------FYSISK NKL TILL INLH (EG. INLI)                                  
020600     03  W-W6D1H1KY-MIN-X.                                                
020700         05  WH1-MIN-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
020800         05  WH1-MIN-IDDC        PIC X(2)     VALUE LOW-VALUE.            
020900         05  WH1-MIN-IDLEVNR     PIC X(5)     VALUE LOW-VALUE.            
021000         05  WH1-MIN-IDFS        PIC X(8)                                 
021100                                              VALUE LOW-VALUE.            
021200         05  WH1-MIN-TIAVIDAT    PIC S9(7)    COMP-3                      
021300                                              VALUE ZERO.                 
021400         05  WH1-MIN-IDRADNR-INL PIC S9(5)    COMP-3                      
021500                                              VALUE ZERO.                 
021600                                                                          
021700     03  W-W6D1H1KY-MAX-X.                                                
021800         05  WH1-MAX-IDARTNR     PIC S9(9)    COMP-3 VALUE ZERO.          
021900         05  WH1-MAX-IDDC        PIC X(2)     VALUE HIGH-VALUE.           
022000         05  WH1-MAX-IDLEVNR     PIC X(5)     VALUE HIGH-VALUE.           
022100         05  WH1-MAX-IDFS        PIC X(8)                                 
022200                                              VALUE HIGH-VALUE.           
022300         05  WH1-MAX-TIAVIDAT    PIC S9(7)    COMP-3                      
022400                                              VALUE 9999999.              
022500         05  WH1-MAX-IDRADNR-INL PIC S9(5)    COMP-3                      
022600                                              VALUE 99999.                
022700     03  W-WDGXKEY-2231-X.                                                
022800         05  FILLER              PIC X(4)     VALUE '2231'.               
022900         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
023000     03  W-WDGXKEY-2232-X.                                                
023100         05  W-IDANSK-L          PIC S9(3)    VALUE ZERO COMP-3.          
023200         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
023300                                                                          
023400     SKIP2                                                                
023500*    --- STATUS-KOD FRÅN IMS                                              
023600 01  STATUS-WS                   PIC XX.                                  
023700     88  SEGMENT-FINNS                       VALUE '  '.                  
023800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
024000     SKIP2                                                                
024100 01  GODK-STATUSKODER.                                                    
024200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024300     SKIP3                                                                
024400 01  SSA1                        PIC X(128).                              
024500 01  SSA2                        PIC X(64).                               
024600 01  SSA3                        PIC X(64).                               
024700 01  SSA4                        PIC X(64).                               
024800     EJECT                                                                
024900*    --- IMS FUNKTIONSKODER                                               
025000*01  -COPY W0003                                                          
025100     EJECT                                                                
025200*    ---  DLI INPUT-OUTPUT AREA                                           
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD9'.                        
025400 01  DLI-IO-WDD9.                                                         
025500     03  IO-AREA    PIC X(100).                                           
025600     03  AREA-WDD901     REDEFINES IO-AREA.                               
025700*      05  -COPY WDD901                                                   
025800     SKIP3                                                                
025900     03  AREA-WDD902     REDEFINES IO-AREA.                               
026000*      05  -COPY WDD902                                                   
026100     EJECT                                                                
026200     03  AREA-WDD905     REDEFINES IO-AREA.                               
026300*      05 -COPY WDD905                                                    
026400     SKIP2                                                                
026500     03  AREA-WDD906     REDEFINES IO-AREA.                               
026600*      05  -COPY WDD906                                                   
026700     EJECT                                                                
026800*    ---  DLI INPUT-OUTPUT AREA                                           
027300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
027400 01  DLI-IO-WDGX2206.                                                     
027500*    03  -COPY WDGX2206                                                   
027600     EJECT                                                                
027700 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D111'.                      
027800 01  DLI-IO-W6D111.                                                       
027900*    03  -COPY W6D111                                                     
028000     EJECT                                                                
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D1H1'.                      
028200 01  DLI-IO-W6D1H1.                                                       
028300*    03  -COPY W6D1H1                                                     
028400     EJECT                                                                
028500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
028600 01  DLI-IO-WDK601.                                                       
028700*    03  -COPY WDK601  -PRE ART-                                          
028800     EJECT                                                                
028900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
029000 01  DLI-IO-WDK722.                                                       
029100*    03  -COPY WDK722                                                     
029200     EJECT                                                                
029300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD906-B'.                    
029400 01  DLI-IO-WDD906-B.                                                     
029500*    03  -COPY WDD906   -PRE B-                                           
029600     EJECT                                                                
029700 01  DLI-IO-AREA-BX.                                                      
029800     03  IO-AREA-BX              PIC X(50)  VALUE SPACE.                  
029900     SKIP3                                                                
030000     03  WLXXBX01 REDEFINES IO-AREA-BX.                                   
030100*        05  -COPY WDGX01     -PRE XXBX-                                  
030200     SKIP3                                                                
030300     03  WLXXBX20 REDEFINES IO-AREA-BX.                                   
030400*        05  -COPY WDGX2232   -PRE XXBX-                                  
030500     EJECT                                                                
030600 01  DLI-IO-AREA-F1          PIC X(100).                                  
030700     SKIP2                                                                
030800*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
030900     EJECT                                                                
030901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
030902 01  DLI-IO-WDF116.                                                       
030903*    03  -COPY WDF116                                                     
030910     EJECT                                                                
030920 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
030930 01  DLI-IO-WDB601.                                                       
030940*    03  -COPY WDB601                                                     
030950     EJECT                                                                
031000 LINKAGE SECTION.                                                         
031100                                                                          
031200                                                                          
031300*01  -COPY W0008  -PRE WDD9-                                              
031400     05  FILLER                  PIC X.                                   
031500                                                                          
031600*01  -COPY W0008  -PRE 2205-                                              
031700     05  FILLER                  PIC X.                                   
031800                                                                          
031900*01  -COPY W0008  -PRE W6D1-                                              
032000     05  FILLER                  PIC X.                                   
032100                                                                          
032200*01  -COPY W0008  -PRE W6D1H1-                                            
032300     05  FILLER                  PIC X.                                   
032400                                                                          
032500*01  -COPY W0008  -PRE WDK6-                                              
032600     05  FILLER                  PIC X.                                   
032700                                                                          
032800*01  -COPY W0008  -PRE WDK7-                                              
032900     05  FILLER                  PIC X.                                   
033000                                                                          
033100*01  -COPY W0008  -PRE WDD9B-                                             
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400*01  -COPY W0008  -PRE XXBX-                                              
033500     05  FILLER                  PIC X.                                   
033600     EJECT                                                                
033700*01  -COPY W0008  -PRE WDF1-                                              
033800     05  FILLER                  PIC X.                                   
033900     EJECT                                                                
033910*01  -COPY W0008  -PRE WDB6-                                              
033920     05  FILLER                  PIC X.                                   
033930     EJECT                                                                
034000 PROCEDURE DIVISION  USING WDD9-PCB 2205-PCB W6D1-PCB W6D1H1-PCB          
034100                           WDK6-PCB WDK7-PCB WDD9B-PCB XXBX-PCB           
034200                           WDF1-PCB WDB6-PCB.                             
034300 MAIN SECTION.                                                            
034400     ENTRY 'DLITCBL' USING WDD9-PCB 2205-PCB W6D1-PCB W6D1H1-PCB          
034500                           WDK6-PCB WDK7-PCB WDD9B-PCB XXBX-PCB           
034600                           WDF1-PCB WDB6-PCB.                             
034700                                                                          
034800     PERFORM A-INIT                                                       
034900                                                                          
035000     PERFORM IMS-GET-WDD9                                                 
035100     PERFORM UNTIL SEGMENT-SLUT                                           
035200       EVALUATE WDD9-SEG-NAME-FB                                          
035300         WHEN 'WDD901'                                                    
035400           PERFORM B-KOLL-WDK6-WDK7                                       
035500         WHEN 'WDD902'                                                    
035600           IF KINA-USA-ARTIKEL                                            
035700              PERFORM C-KOLL-2206-AVIS                                    
035800           END-IF                                                         
035900         WHEN 'WDD905'                                                    
036000           IF KINA-USA-ARTIKEL                                            
036100              PERFORM D-KOLL-WDD905                                       
036200           END-IF                                                         
036300         WHEN 'WDD906'                                                    
036400           IF KINA-USA-ARTIKEL                                            
036700             PERFORM E-KOLL-WDD906                                        
036900           END-IF                                                         
037000       END-EVALUATE                                                       
037100       PERFORM IMS-GET-WDD9                                               
037200     END-PERFORM                                                          
037300     PERFORM Z-FINIT                                                      
037400                                                                          
037500     MOVE ZERO TO RETURN-CODE                                             
037600     GOBACK                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 A-INIT SECTION.                                                          
037910     MOVE 'A-INIT '  TO CURRENT-SECTION                                   
038000                                                                          
038100     OPEN OUTPUT W2219A                                                   
038200                                                                          
038210     ACCEPT W1-TIKLOCK FROM TIME                                          
038230     COMPUTE W1-TIKLOCK-9KOMPL = +999999999 - W1-TIKLOCK                  
038240                                                                          
038300     MOVE FUNCTION CURRENT-DATE (1:8) TO  W1-DAREGDAT                     
038400     COMPUTE W1-DAREGDAT-9KOMPL = 99999999 -                              
038500                                          W1-DAREGDAT                     
038600                                                                          
038700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
038800     MOVE D-AAR       TO DAGENS-DATUM-AAR    DAGENS-DATUM-AA              
038900     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
039000     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
039100     MOVE D-VECKA     TO DAGENS-DATUM-VV                                  
039200     MOVE D-DAGNR     TO DAGENS-DATUM-D                                   
039300     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
039400                                                                          
039500                                                                          
039600     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
039700     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
039800     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
039900                     DAT-O-TIDATUM DAT-KDSVAR                             
040000     IF DAT-KDSVAR-OK                                                     
040100       MOVE DAT-TISEKEL      TO W-AAAAVV-SE                               
040200       MOVE DAT-TIAA-VECKA   TO W-AAAAVV-AA                               
040300       MOVE DAT-TIVV         TO W-AAAAVV-VV                               
040400       MOVE DAT-TID          TO W-TILEVDAG-SVAR                           
040500     ELSE                                                                 
040600       MOVE 22 TO RKOD-ABEND                                              
040700       PERFORM S99-ABEND                                                  
040800     END-IF                                                               
040900     .                                                                    
041000     EJECT                                                                
041100 B-KOLL-WDK6-WDK7   SECTION.                                              
041110     MOVE 'B-KOLL-WDK6-WDK7 '  TO CURRENT-SECTION                         
041200                                                                          
041300     MOVE IDARTNR  TO W-IDARTNR                                           
041400                      W-IDARTNR-D9                                        
041500     MOVE IDDC     TO W-IDDC                                              
041600                      W-IDDC-D9                                           
041610                      SPAR-IDDC                                           
041700                                                                          
041800     IF IDDC NOT = DCS-IDDC                                               
041810        MOVE IDDC  TO W-IDDC-B6                                           
041820        PERFORM IMS-GU-WDB601                                             
041870     END-IF                                                               
041880                                                                          
041900     IF DCS-NDC-CN                                                        
041910     OR (DCS-NDC-NA AND DCS-USA)                                          
042000        MOVE JA TO SW-KINA-USA-ARTIKEL                                    
042100        PERFORM IMS-GU-WDK601                                             
042200        IF SEGMENT-SAKNAS                                                 
042300           DISPLAY '  ' IDARTNR ' ARTNR FINNS EJ *******'                 
042400           MOVE ZERO TO ART-ART-KDPRODSL                                  
042500        END-IF                                                            
042600                                                                          
042700        PERFORM IMS-GU-WDK722                                             
042800        IF SEGMENT-SAKNAS                                                 
042900           MOVE ZERO TO XLAG-IDANSK                                       
043000                        XLAG-KDAVT                                        
043100        END-IF                                                            
043200     ELSE                                                                 
043300        MOVE NEJ TO SW-KINA-USA-ARTIKEL                                   
043400     END-IF                                                               
043500     .                                                                    
043600     EJECT                                                                
043700 C-KOLL-2206-AVIS   SECTION.                                              
043710     MOVE 'C-KOLL-2206-AVIS '  TO CURRENT-SECTION                         
043800                                                                          
043900     MOVE SPACE       TO SW-AVIS                                          
044000                                                                          
044100     MOVE IDLEVNR     TO W-IDLEVNR-2206                                   
044101                         W-IDLEVNR                                        
044110     MOVE SPAR-IDDC   TO W-IDDC-2206                                      
044200     PERFORM IMS-GU-WDGX2206                                              
044300     IF SEGMENT-FINNS AND 2206-FLAVIS = JA                                
044400        MOVE JA       TO SW-AVIS                                          
044500     END-IF                                                               
044600                                                                          
044900     MOVE NEJ         TO SW-WDF1                                          
045000     .                                                                    
045100     EJECT                                                                
045200 D-KOLL-WDD905      SECTION.                                              
045210     MOVE 'D-KOLL-WDD905  '  TO CURRENT-SECTION                           
045300                                                                          
045400     IF AVIS AND KDAVROP = 2                                              
045500*             LEV = LEV AND DAAVROP-AVS = KÖRNINGSDAG-1 AND               
045600*             KOD = 2                                                     
045700        MOVE W-AAAAVV-SVAR    TO W-DAAVROP                                
045800        MOVE W-TILEVDAG-SVAR  TO W-TILEVDAG                               
045900                                                                          
046000        IF SW-WDF1 = NEJ                                                  
046100           PERFORM IMS-GU-WDF101                                          
046200           MOVE JA TO SW-WDF1                                             
046300        END-IF                                                            
046302                                                                          
046310        IF WDF1-FINNS                                                     
046311           MOVE SPAR-IDDC  TO W-IDDC-F16                                  
046320           PERFORM IMS-GU-WDF116                                          
046321           IF SEGMENT-FINNS                                               
046322             MOVE NDC-KVDAGAR-AVIAVV TO F16-NDC-KVDAGAR-AVIAVV            
046323             MOVE NDC-KVDAGAR-INLAVV TO F16-NDC-KVDAGAR-INLAVV            
046324           ELSE                                                           
046325             MOVE ZERO               TO F16-NDC-KVDAGAR-AVIAVV            
046326                                        F16-NDC-KVDAGAR-INLAVV            
046327           END-IF                                                         
046328        END-IF                                                            
046330                                                                          
046400        IF WDF1-FINNS AND F16-NDC-KVDAGAR-AVIAVV > ZERO                   
046500           PERFORM DC-JUSTERA-AVVIKELSE-AVI                               
046600        ELSE                                                              
046700           IF DAAVROP-AVS = W-DAAVROP AND                                 
046800              TILEVDAG    = W-TILEVDAG                                    
046900                                                                          
047000              PERFORM DA-KOLL-W6D111                                      
047100           END-IF                                                         
047200        END-IF                                                            
047300     END-IF                                                               
047400                                                                          
047500     IF KDAVROP = 2                                                       
047600        IF SW-WDF1 = NEJ                                                  
047700           PERFORM IMS-GU-WDF101                                          
047800           MOVE JA TO SW-WDF1                                             
047900        END-IF                                                            
047910        IF WDF1-FINNS                                                     
047911           MOVE SPAR-IDDC  TO W-IDDC-F16                                  
047920           PERFORM IMS-GU-WDF116                                          
047930           IF SEGMENT-FINNS                                               
047940             MOVE NDC-KVDAGAR-AVIAVV TO F16-NDC-KVDAGAR-AVIAVV            
047941             MOVE NDC-KVDAGAR-INLAVV TO F16-NDC-KVDAGAR-INLAVV            
047950           ELSE                                                           
047960             MOVE ZERO               TO F16-NDC-KVDAGAR-AVIAVV            
047961                                        F16-NDC-KVDAGAR-INLAVV            
047970           END-IF                                                         
047980        END-IF                                                            
048000        IF WDF1-FINNS AND F16-NDC-KVDAGAR-INLAVV > ZERO                   
048100           PERFORM DD-JUSTERA-AVVIKELSE-INL                               
048200        ELSE                                                              
048300           IF TIAVRDAT-INL = DAGENS-DATUM                                 
048400               PERFORM DB-KOLL-WDD906                                     
048500           END-IF                                                         
048600        END-IF                                                            
048700     END-IF                                                               
048800                                                                          
048900     MOVE WDD905                TO SPAR-WDD905                            
049000                                                                          
049400     .                                                                    
049500     EJECT                                                                
049600 DA-KOLL-W6D111     SECTION.                                              
049610     MOVE 'DA-KOLL-W6D111 '  TO CURRENT-SECTION                           
049800     MOVE NEJ                 TO SW-IDLOPNRM                              
049900     MOVE ZERO                TO SUM-KVAVIS                               
050000*    FIXA NYCKLAR TILL W6D1-SEQH                                          
050100     MOVE W-IDARTNR           TO WH1-MIN-IDARTNR                          
050200                                 WH1-MAX-IDARTNR                          
050300     MOVE SPAR-IDDC           TO WH1-MIN-IDDC                             
050400                                 WH1-MAX-IDDC                             
050410     MOVE W-IDLEVNR           TO WH1-MIN-IDLEVNR                          
050420                                 WH1-MAX-IDLEVNR                          
050500     PERFORM IMS-GU-W6D1-SEQH                                             
050600     PERFORM UNTIL SEGMENT-SAKNAS                                         
050700*       FIXA NYCKLAR TILL W6D111                                          
050800        MOVE SPAR-IDDC        TO W-IDDC-D1                                
050810        MOVE W-IDLEVNR        TO W-IDLEVNR-D1                             
050900        MOVE SEQH-IDFS        TO W-IDFS                                   
051000        MOVE SEQH-TIAVIDAT    TO W-TIAVIDAT                               
051100        MOVE SEQH-IDRADNR-INL TO W-IDRADNR-INL                            
051200        PERFORM IMS-GU-W6D111                                             
051300        IF SEGMENT-FINNS AND ART-IDLOPNRM = ZERO                          
051400           MOVE JA TO SW-IDLOPNRM                                         
051500           ADD ART-KVAVIS  TO  SUM-KVAVIS                                 
051600        END-IF                                                            
051700        PERFORM IMS-GN-W6D1-SEQH                                          
051800     END-PERFORM                                                          
051900                                                                          
053410     IF ((SW-IDLOPNRM = NEJ) OR                                           
053420        (SUM-KVAVIS        <  KVAVROP))                                   
053430*       SKAPA LARM 220                                                    
053700        MOVE W1-DAREGDAT-9KOMPL TO UT-LAK-DAREGDAT-9KOMPL                 
053900        MOVE W-IDARTNR          TO UT-LAK-IDARTNR                         
054000        MOVE SPAR-IDDC          TO UT-LAK-IDDC                            
054100        MOVE XLAG-IDANSK        TO UT-LAK-IDANSK                          
054200                                   W-IDANSK-L                             
054300        PERFORM IMS-GU-XXBX-2232                                          
054400        IF SEGMENT-FINNS                                                  
054500          MOVE XXBX-2232-IDANSK-LARM                                      
054600                                TO UT-LAK-IDANSK                          
054700        END-IF                                                            
054800        MOVE W-IDLEVNR          TO UT-LAK-IDLEVNR                         
054900        MOVE 220                TO UT-LAK-KDLARM                          
055000        MOVE '*'                TO UT-LAK-FLNYLARM                        
055100        MOVE SUM-KVAVIS         TO UT-LAK-KVAVIS                          
055200        MOVE KVAVROP            TO UT-LAK-KVAVROP                         
055300        MOVE ZERO               TO UT-LAK-TIAAMMDD-AVS                    
055400                                                                          
055500        MOVE 'AAVVD'            TO DAT-KDDATFORM                          
055600        MOVE DAAVROP-AVS        TO WS-AAAAVV                              
055700        MOVE TILEVDAG           TO WS-TILEVDAG                            
055800        MOVE WS-AAVVD           TO DAT-I-TIDATUM                          
055900        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
056000                            DAT-O-TIDATUM DAT-KDSVAR                      
056100        IF DAT-KDSVAR-OK                                                  
056200           MOVE DAT-TIAAMMDD    TO UT-LAK-TIAAMMDD                        
056300        ELSE                                                              
056400           MOVE 23 TO RKOD-ABEND                                          
056500           PERFORM S99-ABEND                                              
056600        END-IF                                                            
056700                                                                          
056800        PERFORM S11-SKRIV-W2219A                                          
056900     END-IF                                                               
057000     .                                                                    
057100     EJECT                                                                
057200 DB-KOLL-WDD906     SECTION.                                              
057210     MOVE 'DB-KOLL-WDD906 '  TO CURRENT-SECTION                           
057300                                                                          
057400     MOVE ZERO                TO SUM-KVAVROP-AVB                          
057500     MOVE NEJ                 TO SW-WDD906-SAKNAS                         
057600*    FIXA NYCKLAR TILL WDD906                                             
057700     MOVE DAAVROP-AVS         TO W-DAAVROP   WS-AAAAVV                    
057800     MOVE TILEVDAG            TO W-TILEVDAG  WS-TILEVDAG                  
057900     PERFORM IMS-GU-WDD906B                                               
058000     IF SEGMENT-SAKNAS                                                    
058100        MOVE JA TO SW-WDD906-SAKNAS                                       
058200     END-IF                                                               
058300     PERFORM UNTIL SEGMENT-SAKNAS                                         
058400        ADD B-KVAVROP-AVB     TO SUM-KVAVROP-AVB                          
058500        PERFORM IMS-GN-WDD906B                                            
058600     END-PERFORM                                                          
058700                                                                          
059800     IF ((SW-WDD906-SAKNAS = JA) OR                                       
059900        ( KVAVROP NOT = ZERO))                                            
060100********                       (KVAVROP + SUM-KVAVROP-AVB))               
060200*       SKAPA LARM 225                                                    
060600        MOVE W1-DAREGDAT-9KOMPL TO UT-LAK-DAREGDAT-9KOMPL                 
060800        MOVE W-IDARTNR          TO UT-LAK-IDARTNR                         
060900        MOVE SPAR-IDDC          TO UT-LAK-IDDC                            
061000        MOVE XLAG-IDANSK        TO UT-LAK-IDANSK                          
061100                                   W-IDANSK-L                             
061200        PERFORM IMS-GU-XXBX-2232                                          
061300        IF SEGMENT-FINNS                                                  
061400          MOVE XXBX-2232-IDANSK-LARM                                      
061500                                TO UT-LAK-IDANSK                          
061600        END-IF                                                            
061700        MOVE W-IDLEVNR          TO UT-LAK-IDLEVNR                         
061800        MOVE 225                TO UT-LAK-KDLARM                          
061900        MOVE '*'                TO UT-LAK-FLNYLARM                        
062000        MOVE SUM-KVAVROP-AVB    TO UT-LAK-KVAVIS                          
062100        MOVE KVAVROP            TO UT-LAK-KVAVROP                         
062200        ADD  SUM-KVAVROP-AVB    TO UT-LAK-KVAVROP                         
062300        MOVE TIAVRDAT-INL       TO UT-LAK-TIAAMMDD                        
062400                                                                          
062500        MOVE 'AAVVD'            TO DAT-KDDATFORM                          
062600        MOVE WS-AAVVD           TO DAT-I-TIDATUM                          
062700        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
062800                            DAT-O-TIDATUM DAT-KDSVAR                      
062900        IF DAT-KDSVAR-OK                                                  
063000           MOVE DAT-TIAAMMDD    TO UT-LAK-TIAAMMDD-AVS                    
063100        ELSE                                                              
063200           MOVE 26 TO RKOD-ABEND                                          
063300           PERFORM S99-ABEND                                              
063400        END-IF                                                            
063500                                                                          
063600        PERFORM S11-SKRIV-W2219A                                          
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000 DC-JUSTERA-AVVIKELSE-AVI SECTION.                                        
064010     MOVE 'DC-JUSTERA-AVVIKELSE-AVI '  TO CURRENT-SECTION                 
064100                                                                          
064200*  JUSTERING GÖRS MED ANTALET DAGAR (X) I KVDAGAR-AVIAVV                  
064300*  DVS ATT VI FÅR JÄMFÖRA MED DAAVROP-AVS  + X                            
064400                                                                          
064500     MOVE DAAVROP-AVS        TO W-JUST-DAAVROP                            
064600     MOVE TILEVDAG           TO W-JUST-TILEVDAG                           
064700     MOVE W-JUST-AAVVD       TO DAT-I-TIDATUM                             
064800     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
064900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
065000                     DAT-O-TIDATUM DAT-KDSVAR                             
065100     IF DAT-KDSVAR-OK                                                     
065200       CONTINUE                                                           
065300     ELSE                                                                 
065400       MOVE 28 TO RKOD-ABEND                                              
065500       PERFORM S99-ABEND                                                  
065600     END-IF                                                               
065700                                                                          
065800     IF DAT-TIAAMMDD > 500000 OR                                          
065900        DAT-TIAAMMDD < 060101                                             
066000        MOVE 060101  TO DAT-TIAAMMDD                                      
066100     END-IF                                                               
066200                                                                          
066300     MOVE 2                  TO WORK-KDCALL                               
066400     MOVE SPAR-IDDC          TO WORK-IDDC                                 
066500     MOVE DAT-TIAAMMDD       TO WORK-TIAAMMDD-FOM                         
066600     MOVE F16-NDC-KVDAGAR-AVIAVV  TO WORK-KVWORKD                         
066700     ADD  +1                 TO WORK-KVWORKD                              
066800     CALL WORKDAY USING WORK-KDCALL                                       
066900               WORK-DATE-AREA WORK-KDSVAR                                 
067000     IF WORK-KDSVAR-OK                                                    
067100       CONTINUE                                                           
067200     ELSE                                                                 
067300       MOVE 27 TO RKOD-ABEND                                              
067400       PERFORM S99-ABEND                                                  
067500     END-IF                                                               
067600                                                                          
067700     IF WORK-TIAAMMDD-TOM = DAGENS-DATUM                                  
067800        PERFORM DA-KOLL-W6D111                                            
067900     END-IF                                                               
068000     .                                                                    
068100     EJECT                                                                
068200 DD-JUSTERA-AVVIKELSE-INL SECTION.                                        
068210     MOVE 'DD-JUSTERA-AVVIKELSE-INL '   TO CURRENT-SECTION                
068300                                                                          
068400*  JUSTERING GÖRS MED ANTALET DAGAR (X) I KVDAGAR-INLAVV                  
068500*  DVS ATT VI FÅR JÄMFÖRA MED TIAVRDAT-INL + X                            
068600                                                                          
068700*    FIX FÖR ATT WORKDAY INTE KLARAR GAMLA DATUM                          
068800     IF TIAVRDAT-INL > 500000 OR TIAVRDAT-INL < 060101                    
068900        MOVE 060101 TO TIAVRDAT-INL                                       
069000     END-IF                                                               
069100*                                                                         
069200     MOVE 2                  TO WORK-KDCALL                               
069300     MOVE SPAR-IDDC          TO WORK-IDDC                                 
069400     MOVE TIAVRDAT-INL       TO WORK-TIAAMMDD-FOM                         
069500     MOVE F16-NDC-KVDAGAR-INLAVV  TO WORK-KVWORKD                         
069600     ADD  +1                 TO WORK-KVWORKD                              
069700     CALL WORKDAY USING WORK-KDCALL                                       
069800               WORK-DATE-AREA WORK-KDSVAR                                 
069900     IF WORK-KDSVAR-OK                                                    
070000       CONTINUE                                                           
070100     ELSE                                                                 
070200       MOVE 29 TO RKOD-ABEND                                              
070300       PERFORM S99-ABEND                                                  
070400     END-IF                                                               
070500                                                                          
070600     IF WORK-TIAAMMDD-TOM = DAGENS-DATUM                                  
070700        PERFORM DB-KOLL-WDD906                                            
070800     END-IF                                                               
070900     .                                                                    
071000     EJECT                                                                
071100 E-KOLL-WDD906      SECTION.                                              
071110     MOVE 'E-KOLL-WDD906 '   TO CURRENT-SECTION                           
071200                                                                          
071300     IF SPAR-KDAVROP = 9 OR 2                                             
071400        MOVE IDLOPNRM-PL  TO WS-IDLOPNRM-PL                               
071500        IF WS-IDLOPNRM-AAVVD = DAGENS-DATUM-AAVVD                         
071600*          TEST DAGENS-DATUM < (SPAR-TIAVRDAT-INL - 4)                    
071700           MOVE 3                  TO WORK-KDCALL                         
071800           MOVE SPAR-IDDC          TO WORK-IDDC                           
072700           MOVE SPAR-TIAVRDAT-INL  TO WORK-TIAAMMDD-TOM                   
072800           MOVE +5                 TO WORK-KVWORKD                        
072900           CALL WORKDAY USING WORK-KDCALL                                 
073000                     WORK-DATE-AREA WORK-KDSVAR                           
073100           IF WORK-KDSVAR-OK                                              
073200             CONTINUE                                                     
073300           ELSE                                                           
073400             MOVE 25 TO RKOD-ABEND                                        
073500             PERFORM S99-ABEND                                            
073600           END-IF                                                         
073700                                                                          
073800           IF DAGENS-DATUM < WORK-TIAAMMDD-FOM                            
073900              PERFORM EA-LARM-TIDIGT                                      
074000           END-IF                                                         
074100        END-IF                                                            
074200     END-IF                                                               
074300     .                                                                    
074400     EJECT                                                                
074500 EA-LARM-TIDIGT SECTION.                                                  
074510     MOVE 'EA-LARM-TIDIGT '   TO CURRENT-SECTION                          
074600                                                                          
074700*       SKAPA LARM 230 ( TO EARLY )                                       
075100        MOVE W1-DAREGDAT-9KOMPL TO UT-LAK-DAREGDAT-9KOMPL                 
075300        MOVE W-IDARTNR          TO UT-LAK-IDARTNR                         
075400        MOVE SPAR-IDDC          TO UT-LAK-IDDC                            
075500        MOVE XLAG-IDANSK        TO UT-LAK-IDANSK                          
075600                                   W-IDANSK-L                             
075700        PERFORM IMS-GU-XXBX-2232                                          
075800        IF SEGMENT-FINNS                                                  
075900          MOVE XXBX-2232-IDANSK-LARM                                      
076000                                TO UT-LAK-IDANSK                          
076100        END-IF                                                            
076200        MOVE W-IDLEVNR          TO UT-LAK-IDLEVNR                         
076300        MOVE 230                TO UT-LAK-KDLARM                          
076400        MOVE '*'                TO UT-LAK-FLNYLARM                        
076500        MOVE KVAVROP-AVB        TO UT-LAK-KVAVIS                          
076600        MOVE SPAR-TIAVRDAT-INL  TO UT-LAK-TIAAMMDD                        
076700        MOVE ZERO               TO UT-LAK-TIAAMMDD-AVS                    
076800        MOVE SPAR-KVAVROP       TO UT-LAK-KVAVROP                         
076900        ADD  KVAVROP-AVB        TO UT-LAK-KVAVROP                         
077000***     (ELLER SKALL SAMTLIGA 06-SEGMS AVB ADDERAS ?)                     
077100                                                                          
077200        PERFORM S11-SKRIV-W2219A                                          
077300     .                                                                    
077400     EJECT                                                                
077500 Z-FINIT SECTION.                                                         
077510     MOVE 'Z-FINIT '   TO CURRENT-SECTION                                 
077600                                                                          
077700     CLOSE W2219A                                                         
077800                                                                          
077900     MOVE 'S' TO POSTSUM-OPKOD                                            
078000     CALL POSTSUM USING POSTSUM-PARM                                      
078100     .                                                                    
078200     EJECT                                                                
078300 S11-SKRIV-W2219A SECTION.                                                
078310     MOVE 'S11-SKRIV-W2219A '    TO CURRENT-SECTION                       
078400                                                                          
078410     COMPUTE W1-TIKLOCK-9KOMPL = W1-TIKLOCK-9KOMPL - 1                    
078420     MOVE W1-TIKLOCK-9KOMPL  TO UT-LAK-TIKLOCK-9KOMPL                     
078430                                                                          
078500     WRITE UT-POST         FROM UT-AREA                                   
078600                                                                          
078700     MOVE 'LARM'             TO POSTSUM-TRANSTYP                          
078800     MOVE 'W2219A'           TO POSTSUM-FDNAMN                            
078900     MOVE 'W2219AD1'         TO POSTSUM-DDNAMN2                           
079000     CALL POSTSUM         USING POSTSUM-PARM                              
079100     .                                                                    
079200     EJECT                                                                
079300 S99-ABEND SECTION.                                                       
079310     MOVE 'S99-ABEND '   TO CURRENT-SECTION                               
079400                                                                          
079500     SKIP2                                                                
079600     MOVE 'S' TO POSTSUM-OPKOD                                            
079700     CALL POSTSUM USING POSTSUM-PARM                                      
079800     CALL ABEND USING RKOD-ABEND                                          
079900     .                                                                    
080000     EJECT                                                                
080100* --- IMS SEKTIONER ---                                                   
080200                                                                          
080300                                                                          
080400 IMS-GET-WDD9   SECTION.                                                  
080410     MOVE 'IMS-GET-WDD9 '    TO DBS-SECTION                               
080500                                                                          
080600     CALL CBLTDLI USING GN WDD9-PCB DLI-IO-WDD9                           
080700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
080800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
080900     PERFORM IMS-STATUSKONTROLL                                           
081000     .                                                                    
081100     EJECT                                                                
083310 IMS-GU-WDGX2206 SECTION.                                                 
083311     MOVE 'IMS-GU-WDGX2206 '   TO DBS-SECTION                             
083320                                                                          
083330     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
083340          DELIMITED BY SIZE INTO SSA1                                     
083350     STRING 'WDGX2206(KY2206   =' W-KY2206-X ')'                          
083360          DELIMITED BY SIZE INTO SSA2                                     
083370     MOVE '  GE' TO GODK-STATUSKODER                                      
083380     CALL CBLTDLI USING GU 2205-PCB DLI-IO-WDGX2206 SSA1 SSA2             
083390     MOVE 2205-STATUS-CODE TO STATUS-WS                                   
083391     PERFORM IMS-STATUSKONTROLL                                           
083392     .                                                                    
083393     EJECT                                                                
083400 IMS-GU-WDD906B SECTION.                                                  
083410     MOVE 'IMS-GU-WDD906B  ' TO DBS-SECTION                               
083500                                                                          
083600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
083700          DELIMITED BY SIZE INTO SSA1                                     
083800     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
083900          DELIMITED BY SIZE INTO SSA2                                     
084000     STRING 'WDD905  (DAAVROP  =' W-DAAVROP-X ')'                         
084100          DELIMITED BY SIZE INTO SSA3                                     
084200     STRING 'WDD906   '                                                   
084300          DELIMITED BY SIZE INTO SSA4                                     
084400     MOVE '  GE' TO GODK-STATUSKODER                                      
084500     CALL CBLTDLI USING GU WDD9B-PCB DLI-IO-WDD906-B SSA1                 
084600                                                     SSA2                 
084700                                                     SSA3                 
084800                                                     SSA4                 
084900     MOVE WDD9B-STATUS-CODE TO STATUS-WS                                  
085000     PERFORM IMS-STATUSKONTROLL                                           
085100     .                                                                    
085200     EJECT                                                                
085300 IMS-GN-WDD906B SECTION.                                                  
085310     MOVE 'IMS-GN-WDD906B   '    TO DBS-SECTION                           
085400                                                                          
085500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
085600          DELIMITED BY SIZE INTO SSA1                                     
085700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
085800          DELIMITED BY SIZE INTO SSA2                                     
085900     STRING 'WDD905  (DAAVROP  =' W-DAAVROP-X ')'                         
086000          DELIMITED BY SIZE INTO SSA3                                     
086100     STRING 'WDD906   '                                                   
086200          DELIMITED BY SIZE INTO SSA4                                     
086300     MOVE '  GE' TO GODK-STATUSKODER                                      
086400     CALL CBLTDLI USING GN WDD9B-PCB DLI-IO-WDD906-B SSA1                 
086500                                                     SSA2                 
086600                                                     SSA3                 
086700                                                     SSA4                 
086800     MOVE WDD9B-STATUS-CODE TO STATUS-WS                                  
086900     PERFORM IMS-STATUSKONTROLL                                           
087000     .                                                                    
087100     EJECT                                                                
087200 IMS-GU-WDK601 SECTION.                                                   
087210     MOVE 'IMS-GU-WDK601   '   TO DBS-SECTION                             
087300                                                                          
087400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
087500          DELIMITED BY SIZE INTO SSA1                                     
087600     MOVE '  GE' TO GODK-STATUSKODER                                      
087700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
087800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
087900     PERFORM IMS-STATUSKONTROLL                                           
088000     .                                                                    
088100     EJECT                                                                
088200 IMS-GU-WDK722 SECTION.                                                   
088210     MOVE 'IMS-GU-WDK722 '   TO DBS-SECTION                               
088300                                                                          
088400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
088500          DELIMITED BY SIZE INTO SSA1                                     
088600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
088700          DELIMITED BY SIZE INTO SSA2                                     
088800     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
088900          DELIMITED BY SIZE INTO SSA3                                     
089000     MOVE '  GE' TO GODK-STATUSKODER                                      
089100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
089200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
089300     PERFORM IMS-STATUSKONTROLL                                           
089400     .                                                                    
089500     EJECT                                                                
089600******************************************************************        
089700*    W6D1-PCB                                                             
089800******************************************************************        
089900     SKIP3                                                                
090000*----------------------------------------------------------------*        
090100 IMS-GU-W6D111    SECTION.                                                
090110     MOVE 'IMS-GU-W6D111  '   TO DBS-SECTION                              
090120                                                                          
090200     STRING 'W6D101  (W6D101KY =' W-W6D101KY-X ')'                        
090300             DELIMITED BY SIZE INTO SSA1                                  
090400     STRING 'W6D111  (IDRADNRI =' W-IDRADNR-INL-X ')'                     
090500          DELIMITED BY SIZE INTO SSA2                                     
090600     MOVE '  GE'            TO GODK-STATUSKODER                           
090700     CALL CBLTDLI USING GU  W6D1-PCB                                      
090800                            DLI-IO-W6D111                                 
090900                            SSA1                                          
091000                            SSA2                                          
091100     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
091200     PERFORM IMS-STATUSKONTROLL                                           
091300     .                                                                    
091400     EJECT                                                                
091500******************************************************************        
091600*    W6D1H1-PCB                                                           
091700******************************************************************        
091800     SKIP3                                                                
091900*----------------------------------------------------------------*        
092000 IMS-GU-W6D1-SEQH     SECTION.                                            
092010     MOVE 'IMS-GU-W6D1-SEQH   '   TO DBS-SECTION                          
092020                                                                          
092100     STRING 'W6D1H1  (W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
092200                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
092300             DELIMITED BY SIZE INTO SSA1                                  
092400     MOVE '  GE'                 TO GODK-STATUSKODER                      
092500     CALL CBLTDLI USING GU       W6D1H1-PCB                               
092600                                 DLI-IO-W6D1H1                            
092700                                 SSA1                                     
092800     MOVE W6D1H1-STATUS-CODE     TO STATUS-WS                             
092900     PERFORM IMS-STATUSKONTROLL                                           
093000     .                                                                    
093100     EJECT                                                                
093200     SKIP3                                                                
093300*----------------------------------------------------------------*        
093400 IMS-GN-W6D1-SEQH     SECTION.                                            
093401     MOVE 'IMS-GN-W6D1-SEQH  '   TO DBS-SECTION                           
093410                                                                          
093500     STRING 'W6D1H1  (W6D1H1KY>=' W-W6D1H1KY-MIN-X                        
093600                    '&W6D1H1KY<=' W-W6D1H1KY-MAX-X ')'                    
093700             DELIMITED BY SIZE INTO SSA1                                  
093800     MOVE '  GE'                 TO GODK-STATUSKODER                      
093900     CALL CBLTDLI USING GN       W6D1H1-PCB                               
094000                                 DLI-IO-W6D1H1                            
094100                                 SSA1                                     
094200     MOVE W6D1H1-STATUS-CODE     TO STATUS-WS                             
094300     PERFORM IMS-STATUSKONTROLL                                           
094400     .                                                                    
094500     EJECT                                                                
094600 IMS-GET-XXBX-2231 SECTION.                                               
094610     MOVE 'IMS-GET-XXBX-2231 '   TO DBS-SECTION                           
094700                                                                          
094800     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
094900          DELIMITED BY SIZE INTO SSA1                                     
095000     MOVE '  ' TO GODK-STATUSKODER                                        
095100     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA-BX SSA1                   
095200     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500                                                                          
095600                                                                          
095700 IMS-GU-XXBX-2232 SECTION.                                                
095710     MOVE 'IMS-GU-XXBX-2232 '  TO DBS-SECTION                             
095800                                                                          
095900     STRING 'WLXXBX01(WDGXKEY  =' W-WDGXKEY-2231-X ')'                    
096000          DELIMITED BY SIZE INTO SSA1                                     
096100     STRING 'WLXXBX11(WDGXKEY  =' W-WDGXKEY-2232-X ')'                    
096200          DELIMITED BY SIZE INTO SSA2                                     
096300     MOVE '  GE' TO GODK-STATUSKODER                                      
096400     CALL CBLTDLI USING GU  XXBX-PCB DLI-IO-AREA-BX SSA1 SSA2             
096500     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
096600     PERFORM IMS-STATUSKONTROLL                                           
096700     .                                                                    
096800     EJECT                                                                
096900 IMS-GU-WDF101 SECTION.                                                   
096910     MOVE 'IMS-GU-WDF101 '   TO DBS-SECTION                               
097000                                                                          
097100     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
097200          DELIMITED BY SIZE INTO SSA1                                     
097300     MOVE '  GE' TO GODK-STATUSKODER                                      
097400     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
097500     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
097600     PERFORM IMS-STATUSKONTROLL                                           
097700     IF SEGMENT-FINNS                                                     
097800        MOVE JA  TO SW-WDF1-FINNS                                         
097900     ELSE                                                                 
098000        MOVE NEJ TO SW-WDF1-FINNS                                         
098100     END-IF                                                               
098200     .                                                                    
098300     EJECT                                                                
098301 IMS-GU-WDF116  SECTION.                                                  
098302     MOVE 'IMS-GU-WDF116  '   TO DBS-SECTION                              
098303                                                                          
098304     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
098305          DELIMITED BY SIZE INTO SSA1                                     
098306     STRING 'WDF116  (IDDC     =' W-IDDC-F16-X     ')'                    
098307          DELIMITED BY SIZE INTO SSA2                                     
098308     MOVE '  GE' TO GODK-STATUSKODER                                      
098309     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
098310     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
098311     PERFORM IMS-STATUSKONTROLL                                           
098318     .                                                                    
098319     EJECT                                                                
098320 IMS-GU-WDB601 SECTION.                                                   
098321     MOVE 'IMS-GU-WDB601 '   TO DBS-SECTION                               
098322                                                                          
098330     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
098340          DELIMITED BY SIZE INTO SSA1                                     
098350     MOVE '    ' TO GODK-STATUSKODER                                      
098360     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
098370     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
098380     PERFORM IMS-STATUSKONTROLL                                           
098395     .                                                                    
098396     EJECT                                                                
098400 IMS-STATUSKONTROLL SECTION.                                              
098500                                                                          
098600     SET STATUS-IX TO 1                                                   
098700     SEARCH GODK-STATUS                                                   
098800       AT END                                                             
098900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
099000           DELIMITED BY SIZE INTO FELTEXT                                 
099100         DISPLAY FELTEXT                                                  
099200         CALL FELLOG                                                      
099300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
099400         CONTINUE                                                         
099500     END-SEARCH                                                           
099600     .                                                                    
