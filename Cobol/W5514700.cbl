000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5514700.                                                
000400 AUTHOR.         KARL JOHAN HANSSON.                                      
000500 DATE-WRITTEN.   94/08/31.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER NER SPISBASEN WDC6.                             
001000*        UTFILEN SORTERAS PÅ IDARTNR.                                     
001100*        OBS !!! OM FÖRSTA LADDNINGEN FÖR ÅRET, DVS OM                    
001200*        LADDATUM PÅ WDR4 ÄR FRÅN FÖRRA ÅRET, SKAPAS TOMFIL.              
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDC6  (WLPRIE)                             
001500*                              WDR4  (WLXXEH)                             
001600*        ÄNDRAD FÖR ETRACKER NO 1499783, INSTALLERAD 2004-10-25           
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP2                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- FIL MED SPISINFO FRÅN WDC6                                 
003000     SELECT W55147                     ASSIGN TO W55147D1.                
003100     SKIP2                                                                
003200*          --- SORTERINGSFIL                                              
003300     SELECT SORTFIL                    ASSIGN TO W55147DS.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W55147                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300*01  POST -COPY W55147 -PRE  UT-  -L.                                     
004400     SKIP3                                                                
004500 SD  SORTFIL                                                              
004600     RECORDING       F                                                    
004700     SKIP2                                                                
004800*01  POST -COPY W55147 -PRE  SORT-                                        
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005200 77  IDPGM                       PIC X(8)    VALUE 'W5514700'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 01  W-TIUPPDAT                  PIC 9(7)    VALUE ZERO.                  
005600 01  DAGENS-DAT-AAR              PIC 9(2)    VALUE ZERO.                  
005700                                                                          
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL POSTSUM                                          
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
007700 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
007800                                 'SORTWS-AREA-START  '.                   
007900     SKIP2                                                                
008000                                                                          
008100*01  AREA -COPY W55147     -PRE SORTWS-                                   
008200     SKIP2                                                                
008300 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
008400     EJECT                                                                
008500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008600*                                                                         
008700     SKIP2                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100     03  W-IDARTNR-X.                                                     
009200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009210     03  W-KDSEGKEY-X.                                                    
009220         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
009230     03  W-KDARBTYP-X.                                                    
009240         05  W-KDARBTYP          PIC X(8)    VALUE 'ANSK    '.            
009250     03  W-IDPERSON-X.                                                    
009260         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
009400     03  W-WDGXKEY-X.                                                     
009500         05  W-IDHTYP            PIC X(4)    VALUE '5119'.                
009600         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
009700     SKIP2                                                                
009800*    --- STATUS-KOD FRÅN IMS                                              
009900 01  STATUS-WS                   PIC XX.                                  
010000     88  SEGMENT-FINNS                       VALUE '  '.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(128).                              
010800 01  SSA2                        PIC X(128).                              
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011500     SKIP3                                                                
011600 01  DLI-IO-AREA.                                                         
011700                                                                          
011800*    03  AREA -COPY WDC601  -PRE PRIE01-                                  
011900     SKIP3                                                                
012000*    03  AREA -COPY WDGX5120  -PRE XXEH-                                  
012010                                                                          
012020 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012030 01  DLI-IO-WDK601.                                                       
012040*    03  -COPY WDK601                                                     
012050 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012060 01  DLI-IO-WDK611.                                                       
012070*    03  -COPY WDK611                                                     
012080 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP301'.                      
012090 01  DLI-IO-WDP301.                                                       
012091*    03  -COPY WDP301                                                     
012092 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
012093 01  DLI-IO-WDP311.                                                       
012094*    03  -COPY WDP311                                                     
012095     EJECT                                                                
012100                                                                          
012200 LINKAGE SECTION.                                                         
012300                                                                          
012400*01  -COPY W0008  -PRE PRIE-                                              
012500     05  FILLER                  PIC X.                                   
012600                                                                          
012700*01  -COPY W0008  -PRE XXEH-                                              
012800     05  FILLER                  PIC X.                                   
012810*01  -COPY W0008  -PRE WDK6-                                              
012820     05  FILLER                  PIC X.                                   
012830*01  -COPY W0008  -PRE WDP3-                                              
012840     05  FILLER                  PIC X.                                   
012900     EJECT                                                                
013000 PROCEDURE DIVISION  USING PRIE-PCB XXEH-PCB WDK6-PCB WDP3-PCB.           
013100     ENTRY 'DLITCBL' USING PRIE-PCB XXEH-PCB WDK6-PCB WDP3-PCB.           
013200                                                                          
013300     PERFORM A-INIT                                                       
013500     SORT SORTFIL ASCENDING KEY SORT-IDARTNR                              
013600                                                                          
013700                  INPUT PROCEDURE B-SORT-INPUT                            
013800                  GIVING W55147                                           
013900                                                                          
014000     IF SORT-RETURN NOT = 0                                               
014100       MOVE SORT-RETURN TO SORT-RETURN-X                                  
014200       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
014300           DELIMITED BY SIZE                                              
014400           INTO FELTEXT-STR                                               
014500       DISPLAY FELTEXT                                                    
014600       PERFORM S99-ABEND                                                  
014700     ELSE                                                                 
014800       PERFORM Z-FINIT                                                    
014900                                                                          
015000       MOVE ZERO TO RETURN-CODE                                           
015100       GOBACK                                                             
015200     END-IF                                                               
015300                                                                          
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT SECTION.                                                          
015800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015900                                                                          
016000     MOVE FUNCTION CURRENT-DATE(3:2) TO DAGENS-DAT-AAR                    
016100                                                                          
016200     PERFORM IMS-GU-WDGX11                                                
016300     IF SEGMENT-FINNS                                                     
016400       MOVE XXEH-5120-TIUPPDAT  TO W-TIUPPDAT                             
016500     ELSE                                                                 
016600       MOVE ZERO                TO W-TIUPPDAT                             
016700     END-IF                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 B-SORT-INPUT SECTION.                                                    
017100                                                                          
017200     DISPLAY '***************************************************'        
017300     DISPLAY 'UTFILEN SKALL VARA TOM VID ÅRETS FÖRSTA KÖRNING'            
017400     DISPLAY 'OM ÅR1 OCH ÅR2 ÄR OLIKA, ÄR UTFILEN TOM: '                  
017500       'ÅR1=' W-TIUPPDAT(2:2) ' ÅR2=' DAGENS-DAT-AAR                      
017600     DISPLAY '***************************************************'        
017610                                                                          
017620                                                                          
017630                                                                          
017800     IF W-TIUPPDAT(2:2) = DAGENS-DAT-AAR                                  
017900       PERFORM IMS-GET-WDC6                                               
018000       PERFORM UNTIL SEGMENT-SLUT                                         
018100         IF PRIE-SEG-NAME-FB = 'WDC601  '                                 
018200             MOVE PRIE01-AREA           TO SORTWS-AREA                    
018300             PERFORM C-READ-WDP3                                          
018400             PERFORM S31-RELEASE-W55147                                   
018500         END-IF                                                           
018600         PERFORM IMS-GET-WDC6                                             
018700       END-PERFORM                                                        
018800     END-IF                                                               
018900     .                                                                    
019000     EJECT                                                                
019001 C-READ-WDP3 SECTION.                                                     
019002                                                                          
019010     MOVE PRIE01-ART-IDARTNR          TO W-IDARTNR                        
019020     PERFORM IMS-GU-WDK611                                                
019030      IF SEGMENT-FINNS                                                    
019040        IF CLAG-IDINK NOT = SPACE                                         
019050          IF CLAG-IDINK (1:3) NUMERIC                                     
019060             MOVE CLAG-IDINK (1:3)    TO W-IDPERSON                       
019070          ELSE                                                            
019080             IF CLAG-IDINK (2:3) NUMERIC                                  
019090                MOVE CLAG-IDINK (2:3) TO W-IDPERSON                       
019091             ELSE                                                         
019092                MOVE ZERO             TO W-IDPERSON                       
019093             END-IF                                                       
019094          END-IF                                                          
019095        MOVE W-IDPERSON    TO SORTWS-IDINK                                
019096        END-IF                                                            
019099      END-IF                                                              
019100     PERFORM IMS-GU-WDP311                                                
019101      IF SEGMENT-FINNS                                                    
019103        MOVE PERS-IDNAMN   TO SORTWS-IDNAMN                               
019104        MOVE PERS-IDMAIL   TO SORTWS-IDMAIL                               
019105        MOVE PERS-IDPERSON TO SORTWS-IDINK                                
019108      ELSE                                                                
019110        MOVE SPACE         TO SORTWS-IDNAMN                               
019111                              SORTWS-IDMAIL                               
019112        MOVE ZERO          TO SORTWS-IDINK                                
019113      END-IF                                                              
019114      .                                                                   
019115      EJECT                                                               
019116                                                                          
019120 Z-FINIT SECTION.                                                         
019200                                                                          
019300     MOVE 'S' TO POSTSUM-OPKOD                                            
019400     CALL POSTSUM USING POSTSUM-PARM                                      
019500     .                                                                    
019600     SKIP2                                                                
019700 S31-RELEASE-W55147  SECTION.                                             
019800                                                                          
019900     RELEASE SORT-POST FROM SORTWS-AREA                                   
020000     .                                                                    
020100     SKIP2                                                                
020200 S99-ABEND SECTION.                                                       
020300                                                                          
020400     MOVE 'S' TO POSTSUM-OPKOD                                            
020500     CALL POSTSUM USING POSTSUM-PARM                                      
020600                                                                          
020700     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
020800     .                                                                    
020900     SKIP3                                                                
021000* --- IMS SEKTIONER ---                                                   
021100     SKIP3                                                                
021200 IMS-GU-WDGX11 SECTION.                                                   
021300                                                                          
021400     STRING 'WLXXEH01(WDGXKEY  =' W-WDGXKEY-X ')'                         
021500          DELIMITED BY SIZE INTO SSA1                                     
021600     MOVE 'WLXXEH11 '         TO SSA2                                     
021700     MOVE '  GE' TO GODK-STATUSKODER                                      
021800     CALL CBLTDLI USING GU XXEH-PCB XXEH-AREA SSA1 SSA2                   
021900     MOVE XXEH-STATUS-CODE TO STATUS-WS                                   
022000     PERFORM IMS-STATUSKONTROLL                                           
022100     .                                                                    
022200     SKIP3                                                                
022300 IMS-GET-WDC6   SECTION.                                                  
022400     SKIP2                                                                
022500     CALL CBLTDLI USING GN PRIE-PCB DLI-IO-AREA                           
022600     MOVE PRIE-STATUS-CODE TO STATUS-WS                                   
022700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
022800     PERFORM IMS-STATUSKONTROLL                                           
022900     .                                                                    
023000     SKIP2                                                                
023010 IMS-GU-WDK611 SECTION.                                                   
023020     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
023030          DELIMITED BY SIZE INTO SSA1                                     
023040     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
023050          DELIMITED BY SIZE INTO SSA2                                     
023060     MOVE '  GE' TO GODK-STATUSKODER                                      
023061     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
023062     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023063     PERFORM IMS-STATUSKONTROLL                                           
023064     .                                                                    
023070     EJECT                                                                
023080                                                                          
023081 IMS-GU-WDP311 SECTION.                                                   
023082     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
023083          DELIMITED BY SIZE INTO SSA1                                     
023084     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
023085          DELIMITED BY SIZE INTO SSA2                                     
023086     MOVE '  GE' TO GODK-STATUSKODER                                      
023087     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
023088     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
023089     PERFORM IMS-STATUSKONTROLL                                           
023090     .                                                                    
023091     EJECT                                                                
023092                                                                          
023100 IMS-STATUSKONTROLL SECTION.                                              
023200                                                                          
023300     SET STATUS-IX TO 1                                                   
023400     SEARCH GODK-STATUS                                                   
023500       AT END                                                             
023600         MOVE 'EJ GODK.STATUS' TO FELTEXT-STR                             
023700         DISPLAY FELTEXT                                                  
023800         CALL FELLOG                                                      
023900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024000         CONTINUE                                                         
024100     END-SEARCH                                                           
024200     .                                                                    
