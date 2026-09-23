020000 ID DIVISION.                                                             
030000 PROGRAM-ID.     W5126800.                                                
040000 AUTHOR.         SARASWATHY S.                                            
050000 DATE-WRITTEN.   19/01/30.                                                
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*                                                                         
090000*    FUNCTION:                                                            
100000*        NEW OBSCOLESCENCE FILE FOR SEPV (COMPANY CODE 57)                
110000*        FOR EACH PART NO SPLIT PER OBSOLESCENCE GROUPS                   
120000*                                                                         
130000*    ABENDCODES:                                                          
140000*        U0016 -  . . . .                                                 
150000*        U1000 -  . . . .                                                 
160000*                                                                         
170000                                                                          
180000     SKIP3                                                                
190000 ENVIRONMENT DIVISION.                                                    
200000     SKIP2                                                                
210000 INPUT-OUTPUT SECTION.                                                    
220000                                                                          
230000 FILE-CONTROL.                                                            
240100     SKIP2                                                                
240200*          --- PARTS STOCK FILE                                           
240300     SELECT W51264B                    ASSIGN TO W51268D1.                
240400     SKIP2                                                                
240500*          --- GIT-INFO-FILE                                              
240600     SELECT W51266                     ASSIGN TO W51268D2.                
240700     SKIP2                                                                
241100*          --- VCCS RECORDS WITH QUANTITY AS PER COMPANY                  
241200     SELECT W51295B                    ASSIGN TO W51268D3.                
241300     SKIP2                                                                
241400*          --- UT FILE WITH STOCKVALUES AND TURNOVER GROUP                
241500*          --- OBSCOLESCENCE FILE FOR SEPV                                
242000     SELECT W51268                     ASSIGN TO W51268D4.                
260000     EJECT                                                                
270000 DATA DIVISION.                                                           
280000     SKIP3                                                                
290000 FILE SECTION.                                                            
300100     SKIP3                                                                
300200 FD  W51264B                                                              
300300     RECORDING       F                                                    
300400     BLOCK CONTAINS  0.                                                   
300500                                                                          
300600*01  -COPY W51264B     -L.                                                
300700     SKIP3                                                                
300800 FD  W51266                                                               
300900     RECORDING       F                                                    
301000     BLOCK CONTAINS  0.                                                   
301100                                                                          
301200*01  -COPY W51266      -L.                                                
301300     SKIP3                                                                
302000 FD  W51295B                                                              
302100     RECORDING       F                                                    
302200     BLOCK CONTAINS  0.                                                   
302300                                                                          
302400*01  -COPY W51295      -L.                                                
302500     SKIP3                                                                
302600 FD  W51268                                                               
302700     RECORDING       F                                                    
302800     BLOCK CONTAINS  0.                                                   
302900                                                                          
302910 01  UT-POST                 PIC X(185).                                  
310000     EJECT                                                                
320000 WORKING-STORAGE SECTION.                                                 
322000                                                                          
323000*    -COPY WY2000W2                                                       
329300     SKIP3                                                                
329400 77  IDPGM                   PIC X(8)      VALUE 'W5126800'.              
329500 77  JA                      PIC X         VALUE 'J'.                     
329600 77  NEJ                     PIC X         VALUE 'N'.                     
330000 77  EOF-W51264B-SW          PIC X         VALUE 'N'.                     
330100     88 EOF-W51264B                        VALUE 'J'.                     
330200     EJECT                                                                
330300 77  EOF-W51266-SW           PIC X         VALUE 'N'.                     
330400     88 EOF-W51266                         VALUE 'J'.                     
330500     EJECT                                                                
330600 77  EOF-W51295B-SW           PIC X         VALUE 'N'.                    
330700     88 EOF-W51295B                         VALUE 'J'.                    
330800     EJECT                                                                
332800 77  OITAB-ARTNR-CDC         PIC S9(9)     COMP-3.                        
333101 77  WS-IX                   PIC S9(3)     COMP-3 VALUE ZERO.             
333301 77  MAX-WEEK                PIC S9(3)     COMP-3 VALUE +53.              
333401                                                                          
333501 01  W-TIAAVVD               PIC S9(5).                                   
333601 01  W-TIAAVVD-P             PIC S9(5)     VALUE +0  COMP-3.              
333701 01  LAGERVARDE              PIC S9(9)     VALUE +0  COMP-3.              
333801 01  TOT-SUARTSTD            PIC S9(11)V99 VALUE +0  COMP-3.              
333901 01  W-SUSTDOI-AR            PIC S9(11)V99 VALUE +0  COMP-3.              
334401 01  W-KVAVIS                PIC S9(11)    VALUE +0  COMP-3.              
334402 01  W-SULEVANT              PIC S9(11)    VALUE +0  COMP-3.              
336301     EJECT                                                                
336401 01  WDATUM                  PIC X(6)      VALUE 'WDATUM'.                
336501                                                                          
336601 01  SUBPROGRAM.                                                          
336701     03  DATKORT             PIC X(8)      VALUE 'DATKORT'.               
336801     03  POSTSUM             PIC X(8)      VALUE 'POSTSUM'.               
336901     03  ABEND               PIC X(8)      VALUE 'ABEND  '.               
337001                                                                          
337101 01  LB-TRANSID.                                                          
337201     03  FILLER              PIC X(7)      VALUE 'W51264B'.               
337301     03  FILLER              PIC X(8)      VALUE 'W51268D1'.              
337401     03  FILLER              PIC X(4)      VALUE ' LB '.                  
337501                                                                          
337601 01  GIT-TRANSID.                                                         
337701     03  FILLER              PIC X(6)      VALUE 'W51266'.                
337801     03  FILLER              PIC X(8)      VALUE 'W51268D2'.              
337901     03  FILLER              PIC X(4)      VALUE ' GIT'.                  
338501                                                                          
338601 01  SALES-TRANSID.                                                       
338701     03  FILLER              PIC X(7)      VALUE 'W51295B'.               
338801     03  FILLER              PIC X(8)      VALUE 'W51268D4'.              
338901     03  FILLER              PIC X(4)      VALUE 'SALE'.                  
339001     EJECT                                                                
339101*   -COPY W0005  -PRE POSTSUM-                                            
342101     EJECT                                                                
342201*   -COPY WDATKORT                                                        
347701     EJECT                                                                
347801 01  FILLER                  PIC X(16)   VALUE 'LB-AREA    '.             
347901*01  LBAREA     -COPY W51264B   -PRE LB-.                                 
350701     EJECT                                                                
350801 01  FILLER                  PIC X(16)   VALUE 'GIT-AREA    '.            
350901*01  GITAREA    -COPY W51266    -PRE GIT-.                                
353301     EJECT                                                                
353401 01  FILLER                  PIC X(16)   VALUE 'IN-AREA     '.            
353501*01  INAREA     -COPY W51295    -PRE SALES-.                              
354701     EJECT                                                                
372402 01  FILLER                  PIC X(16)   VALUE 'UT-AREA    '.             
372403*01  AREA       -COPY W51268    -PRE UT-.                                 
388801     EJECT                                                                
388901                                                                          
389001 01  W-KGR-TABLE.                                                         
389101*                                    **  TABLE FOR STOCKVALUE             
389203     03  W-KGR OCCURS 7.                                                  
389403         05  W-VARDE        PIC S9(11)V99   COMP-3.                       
389501     SKIP3                                                                
389509*                                                                         
391001 01  TABELLINDEX.                                                         
391101     03  KGR-NY              PIC S9         COMP-3.                       
391301     SKIP2                                                                
391401 01  FILLER                  PIC X(16)      VALUE 'W-KURTAB   '.          
391501 01  W-KURTAB.                                                            
391601     03  W-KUR1              PIC S9(11)V99  COMP-3.                       
391701     03  W-KUR2              PIC S9(11)V99  COMP-3.                       
391801     03  W-KUR3              PIC S9(11)V99  COMP-3.                       
391901     03  W-KUR4              PIC S9(11)V99  COMP-3.                       
392001     03  W-KUR5              PIC S9(11)V99  COMP-3.                       
392101     03  W-KUR6              PIC S9(11)V99  COMP-3.                       
392201     03  W-KUR1-GRANS        PIC S9(11)V99  COMP-3.                       
392301     03  W-KUR2-GRANS        PIC S9(11)V99  COMP-3.                       
392401     03  W-KUR3-GRANS        PIC S9(11)V99  COMP-3.                       
392501     03  W-KUR4-GRANS        PIC S9(11)V99  COMP-3.                       
392601     03  W-KUR5-GRANS        PIC S9(11)V99  COMP-3.                       
392701                                                                          
392801     EJECT                                                                
392901 PROCEDURE DIVISION.                                                      
393001                                                                          
393101 MAIN SECTION.                                                            
393201                                                                          
393301     PERFORM A-INIT                                                       
393401                                                                          
393501     PERFORM S01-READ-W51264B-LB                                          
393601     PERFORM S02-READ-W51295B                                             
393801     PERFORM S04-READ-W51266-WDL6                                         
393901                                                                          
394001     PERFORM UNTIL EOF-W51264B                                            
394101         PERFORM B-READ-OI                                                
394201         PERFORM C-CHECK-GIT-PART                                         
394301         PERFORM D-CHECK-KGR-GRP                                          
394401         PERFORM S01-READ-W51264B-LB                                      
394501     END-PERFORM                                                          
394801     PERFORM Z-FINIT                                                      
394901                                                                          
395001     MOVE ZERO TO RETURN-CODE                                             
395101     GOBACK                                                               
395201     .                                                                    
395301     EJECT                                                                
395401                                                                          
395501 A-INIT SECTION.                                                          
395601     OPEN INPUT  W51264B                                                  
395701                 W51266                                                   
395801                 W51295B                                                  
396001          OUTPUT W51268                                                   
396101                                                                          
396201     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
396301                                                                          
396401     CALL DATKORT           USING IDPGM WDATUM DATUMKORT                  
396501     MOVE D-AAR                TO W-TIAAVVD(1:2)                          
396601     MOVE D-VECKA              TO W-TIAAVVD(3:2)                          
396701     MOVE 1                    TO W-TIAAVVD(5:1)                          
396801     MOVE W-TIAAVVD            TO W-TIAAVVD-P                             
396901                                                                          
397001     INITIALIZE W-KGR-TABLE                                               
397301     .                                                                    
397401     EJECT                                                                
397501                                                                          
397601 B-READ-OI SECTION.                                                       
397701                                                                          
397801     MOVE +0                   TO OITAB-ARTNR-CDC                         
398001                                  W-SULEVANT                              
398101                                                                          
398301     PERFORM BA-READ-SALES                                                
398701     .                                                                    
398801     EJECT                                                                
398901                                                                          
399001 BA-READ-SALES SECTION.                                                   
399101                                                                          
399201     PERFORM UNTIL EOF-W51295B  OR SALES-IDARTNR > LB-IDARTNR             
399301       IF SALES-IDARTNR = LB-IDARTNR                                      
399401         IF SALES-SULEVANT  < +0                                          
399501           MOVE +0               TO W-SULEVANT                            
399601         ELSE                                                             
399701           ADD  SALES-SULEVANT    TO W-SULEVANT                           
399801         END-IF                                                           
399901         MOVE LB-IDARTNR         TO OITAB-ARTNR-CDC                       
400001       END-IF                                                             
400101       PERFORM S02-READ-W51295B                                           
400201     END-PERFORM                                                          
400301     .                                                                    
400401     EJECT                                                                
403901 C-CHECK-GIT-PART SECTION.                                                
404001                                                                          
404101     MOVE +0                      TO W-KVAVIS                             
404201     PERFORM UNTIL EOF-W51266                                             
404303                OR GIT-IDARTNR  > LB-IDARTNR                              
404401         IF GIT-IDARTNR = LB-IDARTNR                                      
404501            ADD  GIT-KVAVIS       TO W-KVAVIS                             
404601         END-IF                                                           
404701         PERFORM S04-READ-W51266-WDL6                                     
404801     END-PERFORM                                                          
404901     .                                                                    
405001     EJECT                                                                
405101                                                                          
405201 D-CHECK-KGR-GRP SECTION.                                                 
407901     COMPUTE TOT-SUARTSTD = LB-PRARTSTD *                                 
408001                                  ( LB-KVANTAL + W-KVAVIS )               
408101* 'LB-TIFINLV + 2001 > 'DAGENS DATUM' =>                                  
408201*  => PUBLICERINGSVECKA YNGRE ÄN TVÅ ÅR                                   
408301     MOVE LB-TIFINLV           TO TMP1-YYWWD                              
408401     MOVE W-TIAAVVD-P          TO TMP2-YYWWD                              
408501     PERFORM WY2000P2                                                     
408601     IF LB-KDERS > +20                                                    
408604       MOVE +6                 TO KGR-NY                                  
408606       PERFORM S05-ADD-GIVEN-KURANS                                       
408607     ELSE                                                                 
409001       IF TMP1-YYWWD > +0                                                 
409101         IF (TMP1-YYWWD + 2001) > TMP2-YYWWD                              
409201           MOVE +1             TO KGR-NY                                  
409301           PERFORM S05-ADD-GIVEN-KURANS                                   
409401         ELSE                                                             
409501           IF LB-KVPB = +0 AND LB-KDERS = +0                              
409602             IF W-SULEVANT = +0                                           
409603               MOVE +5         TO KGR-NY                                  
409604               PERFORM S05-ADD-GIVEN-KURANS                               
409605             ELSE                                                         
409606               PERFORM DAA-TEST-BEST-KGR                                  
409607               PERFORM S06-ADD-KUR-SHELF-LIFE-OI                          
409608             END-IF                                                       
409801           ELSE                                                           
409901             IF LB-IDARTNR NOT = OITAB-ARTNR-CDC                          
410001               MOVE +5         TO KGR-NY                                  
410101               PERFORM S05-ADD-GIVEN-KURANS                               
410201             ELSE                                                         
410301               IF W-SULEVANT = +0 AND LB-KDKG NOT = +5                    
410401                 MOVE +5       TO KGR-NY                                  
410501                 PERFORM S05-ADD-GIVEN-KURANS                             
410601               ELSE                                                       
410701                 PERFORM DAA-TEST-BEST-KGR                                
410801                 PERFORM S06-ADD-KUR-SHELF-LIFE-OI                        
410901               END-IF                                                     
411001             END-IF                                                       
411101           END-IF                                                         
411201         END-IF                                                           
411301       ELSE                                                               
411401         MOVE +1               TO KGR-NY                                  
411501         PERFORM S05-ADD-GIVEN-KURANS                                     
411601       END-IF                                                             
411701     END-IF                                                               
411801     .                                                                    
411901     EJECT                                                                
412001                                                                          
412101 DAA-TEST-BEST-KGR SECTION.                                               
412201*****        TESTAR LAGERVÄRDE MOT FÖRBRUKNING               **           
412301     COMPUTE LAGERVARDE = ( LB-KVANTAL + W-KVAVIS)                        
412401     IF LAGERVARDE <= W-SULEVANT                                          
412501               MOVE +1         TO KGR-NY                                  
412601     ELSE                                                                 
412701       IF LAGERVARDE <= (W-SULEVANT * 3)                                  
412801               MOVE +2         TO KGR-NY                                  
412901       ELSE                                                               
413001         IF LAGERVARDE <= (W-SULEVANT * 5)                                
413101               MOVE +3         TO KGR-NY                                  
413201         ELSE                                                             
413301           IF LAGERVARDE <= (W-SULEVANT * 10)                             
413401               MOVE +4         TO KGR-NY                                  
413501           ELSE                                                           
413601               MOVE +5         TO KGR-NY                                  
413701           END-IF                                                         
413801         END-IF                                                           
413901       END-IF                                                             
414001     END-IF                                                               
414101     .                                                                    
414201     EJECT                                                                
414202                                                                          
442801 Z-FINIT SECTION.                                                         
442901                                                                          
443001     CLOSE W51264B                                                        
443002           W51266                                                         
443101           W51295B                                                        
443201           W51268                                                         
443501                                                                          
443601     MOVE 'S'                TO POSTSUM-OPKOD                             
443701     CALL POSTSUM         USING POSTSUM-PARM                              
443801     .                                                                    
443901     EJECT                                                                
444001                                                                          
444101 S01-READ-W51264B-LB SECTION.                                             
444201     READ W51264B           INTO LB-LBAREA                                
444301     AT END                                                               
444401       MOVE JA               TO EOF-W51264B-SW                            
444603     NOT AT END                                                           
445101       MOVE LB-TRANSID       TO POSTSUM-TRANSID                           
445201       CALL POSTSUM       USING POSTSUM-PARM                              
445301     END-READ                                                             
445401     .                                                                    
445501     EJECT                                                                
445601                                                                          
445701 S02-READ-W51295B SECTION.                                                
445801     READ W51295B          INTO SALES-INAREA                              
445901     AT END                                                               
446001       MOVE JA               TO EOF-W51295B-SW                            
446203     NOT AT END                                                           
446601       MOVE SALES-TRANSID     TO POSTSUM-TRANSID                          
446701       CALL POSTSUM       USING POSTSUM-PARM                              
446801     END-READ                                                             
446901     .                                                                    
447001     SKIP2                                                                
447101                                                                          
448701 S04-READ-W51266-WDL6 SECTION.                                            
448801     READ W51266           INTO GIT-GITAREA                               
448901     AT END                                                               
449001       MOVE JA               TO EOF-W51266-SW                             
449203     NOT AT END                                                           
449601       MOVE GIT-TRANSID      TO POSTSUM-TRANSID                           
449701       CALL POSTSUM       USING POSTSUM-PARM                              
449801     END-READ                                                             
449901     .                                                                    
450001     EJECT                                                                
450101                                                                          
450201 S05-ADD-GIVEN-KURANS SECTION.                                            
450303     INITIALIZE W-KGR-TABLE                                               
450403     ADD TOT-SUARTSTD         TO W-VARDE(KGR-NY)                          
450420     PERFORM S07-WRITE-UTFIL                                              
450503     .                                                                    
450601     EJECT                                                                
450701                                                                          
450801 S06-ADD-KUR-SHELF-LIFE-OI SECTION.                                       
450806     INITIALIZE W-KGR-TABLE                                               
451101     COMPUTE W-SUSTDOI-AR = W-SULEVANT * LB-PRARTSTD                      
451501     MULTIPLY W-SUSTDOI-AR BY +1  GIVING W-KUR1-GRANS W-KUR1              
451601     MULTIPLY W-SUSTDOI-AR BY +3  GIVING W-KUR2-GRANS                     
451701     MULTIPLY W-SUSTDOI-AR BY +5  GIVING W-KUR3-GRANS W-KUR4              
451801     MULTIPLY W-SUSTDOI-AR BY +10 GIVING W-KUR4-GRANS                     
451901     MULTIPLY W-SUSTDOI-AR BY +2  GIVING              W-KUR3              
452001                                                      W-KUR2              
452101     MOVE ZERO                        TO              W-KUR5              
452320     IF TOT-SUARTSTD > W-KUR4-GRANS                                       
452401             SUBTRACT W-KUR4-GRANS FROM TOT-SUARTSTD GIVING W-KUR5        
452501     ELSE                                                                 
452601       MOVE ZERO                                       TO W-KUR5          
452701       IF TOT-SUARTSTD > W-KUR3-GRANS                                     
452801             SUBTRACT W-KUR3-GRANS FROM TOT-SUARTSTD GIVING W-KUR4        
452806                                                                          
452901       ELSE                                                               
453001         MOVE ZERO                                     TO W-KUR4          
453101         IF TOT-SUARTSTD > W-KUR2-GRANS                                   
453201             SUBTRACT W-KUR2-GRANS FROM TOT-SUARTSTD GIVING W-KUR3        
453301         ELSE                                                             
453401           MOVE ZERO                                   TO W-KUR3          
453501           IF TOT-SUARTSTD > W-KUR1-GRANS                                 
453601             SUBTRACT W-KUR1-GRANS FROM TOT-SUARTSTD GIVING W-KUR2        
453701           ELSE                                                           
453801             MOVE ZERO                                 TO W-KUR2          
453901             MOVE TOT-SUARTSTD                         TO W-KUR1          
454001           END-IF                                                         
454101         END-IF                                                           
454201       END-IF                                                             
454301     END-IF                                                               
454503     ADD W-KUR1                      TO W-VARDE(1)                        
454603     ADD W-KUR2                      TO W-VARDE(2)                        
454703     ADD W-KUR3                      TO W-VARDE(3)                        
454803     ADD W-KUR4                      TO W-VARDE(4)                        
454903     ADD W-KUR5                      TO W-VARDE(5)                        
454920     PERFORM S07-WRITE-UTFIL                                              
455001     .                                                                    
455101     EJECT                                                                
455201                                                                          
455303 S07-WRITE-UTFIL SECTION.                                                 
455403     MOVE LB-IDARTNR                 TO UT-IDARTNR                        
455503     MOVE LB-PRARTSTD                TO UT-PRARTSTD                       
455603     MOVE LB-KDPRODSL                TO UT-KDPRODSL                       
455703     COMPUTE UT-KVANTAL = LB-KVANTAL + W-KVAVIS                           
455704     MOVE W-SULEVANT                 TO UT-SULEVANT                       
455903     MOVE LB-TIFINLV                 TO UT-TIFINLV                        
456003     MOVE LB-KDERS                   TO UT-KDERS                          
456103     MOVE LB-KVPB                    TO UT-KVPB                           
456203     MOVE KGR-NY                     TO UT-KDKG                           
456303     MOVE W-VARDE(1)                 TO UT-STOCKVALUE1                    
456403     MOVE W-VARDE(2)                 TO UT-STOCKVALUE2                    
456503     MOVE W-VARDE(3)                 TO UT-STOCKVALUE3                    
456603     MOVE W-VARDE(4)                 TO UT-STOCKVALUE4                    
456703     MOVE W-VARDE(5)                 TO UT-STOCKVALUE5                    
456803     MOVE W-VARDE(6)                 TO UT-STOCKVALUE6                    
456816     WRITE UT-POST                   FROM UT-AREA                         
456817     MOVE SPACE                      TO POSTSUM-TRANSTYP                  
456818     MOVE 'UTFIL'                    TO POSTSUM-FDNAMN                    
456819     MOVE 'W51268D5'                 TO POSTSUM-DDNAMN2                   
456820     CALL POSTSUM                    USING POSTSUM-PARM                   
456821     .                                                                    
456830     EJECT                                                                
456903*    -COPY WY2000P2                                                       
