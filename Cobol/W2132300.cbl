000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2132300.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   10/12/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SKRIVER LEVERANTÖRLISTA                                          
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- NEDLÄST LEVREG  WDF1                                       
002500     SELECT W21323                     ASSIGN TO W21323D1.                
002600     SKIP2                                                                
002700*          --- LISTA LEVERANTÖRREGISTER                                   
002800     SELECT W21324                     ASSIGN TO W21323D2.                
002900     SKIP2                                                                
003000*          --- SORTERINGSFIL                                              
003100     SELECT SORTFIL                    ASSIGN TO W21323DS.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W21323                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W21323      -L.                                                
004200     SKIP3                                                                
004300 FD  W21324                                                               
004400     RECORDING       V                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700 01  UT-POST           PIC X(120).                                        
004800     SKIP2                                                                
004900 SD  SORTFIL.                                                             
005000                                                                          
005100*01  POST -COPY W21323      -PRE SORT-                                    
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500 77  IDPGM                       PIC X(8)    VALUE 'W2132300'.            
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800                                                                          
005900 77  W21323-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W21323                       VALUE 'J'.                   
006100                                                                          
006200 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
006300     88  END-OF-SORTFIL                      VALUE 'J'.                   
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000 01  ANTAL-RAD                   PIC S9(3).                               
007100 01  ANTAL-SID                   PIC S9(5).                               
007200 01  IX                          PIC S9(3).                               
007300 01  FL-SKRIV-RADER              PIC X   VALUE 'N'.                       
007400 01  FL-RAD-DEL1-OK              PIC X   VALUE 'N'.                       
007500     EJECT                                                                
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007700*                                                                         
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     SKIP2                                                                
008100*    --- PARAMETRAR TILL ABEND                                            
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008600     SKIP2                                                                
008700 01  FELTEXT.                                                             
008800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL POSTSUM                                          
009200*                                                                         
009300*01  -COPY W0005   -PRE  POSTSUM-                                         
009400     EJECT                                                                
009500 01  IN-AREA-START               PIC X(24)   VALUE                        
009600                                 'IN-AREA-START  '.                       
009700     SKIP2                                                                
009800                                                                          
009900*01  AREA -COPY W21323     -PRE INS-                                      
010000                                                                          
010100     EJECT                                                                
010200 01  UT-AREA-START               PIC X(24)   VALUE                        
010300                                 'UT-AREA-START  '.                       
010400     SKIP2                                                                
010500 01  UT-AREA.                                                             
010600     03  UT-AREA-0.                                                       
010700       05  FILLER                PIC X(120).                              
010800                                                                          
010900 01  RUBRIK.                                                              
011000     03  FILLER                  PIC X(10)  VALUE ' VCCS '.               
011100     03  FILLER                  PIC X(15)  VALUE 'W21323-001'.           
011200     03  FILLER                  PIC X(30)                                
011300                                 VALUE 'LEVERANTÖRSLISTA VCCS  '.         
011400     03  FILLER                  PIC X(06)  VALUE 'LEVNR '.               
011500     03  RUB-IDLEVNR             PIC X(05).                               
011600     03  FILLER                  PIC X(10)  VALUE '   DATUM  '.           
011700     03  RUB-DATUM               PIC X(06).                               
011800     03  FILLER                  PIC X(10)  VALUE '    SIDA  '.           
011900     03  RUB-SIDA                PIC Z(06).                               
012000                                                                          
012100 01  RAD1.                                                                
012200     03  RAD1-1.                                                          
012300         05  FILLER              PIC X(15) VALUE ' LEVERANTÖRNR:'.        
012400         05  RAD1-IDLEVNR1       PIC X(5).                                
012500         05  FILLER              PIC X(36)  VALUE SPACE.                  
012600     03  RAD1-2.                                                          
012700         05  FILLER              PIC X(14)  VALUE 'LEVERANTÖRNR:'.        
012800         05  RAD1-IDLEVNR2       PIC X(5).                                
012900         05  FILLER              PIC X(36)  VALUE SPACE.                  
013000                                                                          
013100 01  RAD2.                                                                
013200     03  RAD2-1.                                                          
013300         05  FILLER              PIC X(07)  VALUE ' NAMN:'.               
013400         05  RAD2-BELEV1         PIC X(35).                               
013500         05  FILLER              PIC X(14)  VALUE SPACE.                  
013600     03  RAD2-2.                                                          
013700         05  FILLER              PIC X(06)  VALUE 'NAMN:'.                
013800         05  RAD2-BELEV2         PIC X(35).                               
013900         05  FILLER              PIC X(14)  VALUE SPACE.                  
014000                                                                          
014100 01  RAD3.                                                                
014200     03  RAD3-1.                                                          
014300         05  FILLER              PIC X(07)  VALUE ' ADR: '.               
014400         05  RAD3-ADLEV1         PIC X(35).                               
014500         05  FILLER              PIC X(14)  VALUE SPACE.                  
014600     03  RAD3-2.                                                          
014700         05  FILLER              PIC X(06)  VALUE 'ADR: '.                
014800         05  RAD3-ADLEV2         PIC X(35).                               
014900         05  FILLER              PIC X(14)  VALUE SPACE.                  
015000                                                                          
015100 01  RAD4.                                                                
015200     03  RAD4-1.                                                          
015300         05  FILLER              PIC X(07)  VALUE ' ADR2:'.               
015400         05  RAD4-ADLEV1         PIC X(35).                               
015500         05  FILLER              PIC X(14)  VALUE SPACE.                  
015600     03  RAD4-2.                                                          
015700         05  FILLER              PIC X(06)  VALUE 'ADR2:'.                
015800         05  RAD4-ADLEV2         PIC X(35).                               
015900         05  FILLER              PIC X(14)  VALUE SPACE.                  
016000                                                                          
016100 01  RAD5.                                                                
016200     03  RAD5-1.                                                          
016300         05  FILLER              PIC X(07)  VALUE ' PADR:'.               
016400         05  RAD5-ADLEV-ORT1     PIC X(35).                               
016500         05  FILLER              PIC X(14)  VALUE SPACE.                  
016600     03  RAD5-2.                                                          
016700         05  FILLER              PIC X(06)  VALUE 'PADR:'.                
016800         05  RAD5-ADLEV-ORT2     PIC X(35).                               
016900         05  FILLER              PIC X(14)  VALUE SPACE.                  
017000                                                                          
017100 01  RAD6.                                                                
017200     03  RAD6-1.                                                          
017300         05  FILLER              PIC X(07)  VALUE ' LAND:'.               
017400         05  RAD6-ADLEVLND1      PIC X(35).                               
017500         05  FILLER              PIC X(14)  VALUE SPACE.                  
017600     03  RAD6-2.                                                          
017700         05  FILLER              PIC X(06)  VALUE 'LAND:'.                
017800         05  RAD6-ADLEVLND2      PIC X(35).                               
017900         05  FILLER              PIC X(14)  VALUE SPACE.                  
018000                                                                          
018100 01  RAD7.                                                                
018200     03  RAD7-1.                                                          
018300         05  FILLER              PIC X(07)  VALUE ' TEL: '.               
018400         05  RAD7-IDLEVTLF1      PIC X(35).                               
018500         05  FILLER              PIC X(14)  VALUE SPACE.                  
018600     03  RAD7-2.                                                          
018700         05  FILLER              PIC X(06)  VALUE 'TEL: '.                
018800         05  RAD7-IDLEVTLF2      PIC X(35).                               
018900         05  FILLER              PIC X(14)  VALUE SPACE.                  
019000                                                                          
019100 01  RAD8.                                                                
019110     03  RAD8-1.                                                          
019111         05  FILLER              PIC X(01)  VALUE SPACE.                  
019112         05  RAD8-IDATTENT1      PIC ZZ.                                  
019120         05  FILLER              PIC X(01)  VALUE SPACE.                  
019130         05  RAD8-BELEV1         PIC X(35).                               
019140         05  FILLER              PIC X(17)  VALUE SPACE.                  
019150     03  RAD8-2.                                                          
019152         05  RAD8-IDATTENT2      PIC ZZ.                                  
019153         05  FILLER              PIC X(01)  VALUE SPACE.                  
019154         05  RAD8-BELEV2         PIC X(35).                               
019155         05  FILLER              PIC X(17)  VALUE SPACE.                  
019190                                                                          
019200 01  RAD9.                                                                
019300     03  RAD9-1.                                                          
019400         05  FILLER              PIC X(01)  VALUE SPACE.                  
019600         05  RAD9-IDMAIL1        PIC X(50).                               
019700         05  FILLER              PIC X(05)  VALUE SPACE.                  
019710     03  RAD9-2.                                                          
019740         05  RAD9-IDMAIL2        PIC X(50).                               
019750         05  FILLER              PIC X(05)  VALUE SPACE.                  
019760                                                                          
019800 01  RAD10.                                                               
019900     03  RAD10-1.                                                         
020010         05  FILLER               PIC X(01)  VALUE SPACE.                 
020100         05  RAD10-IDLEVTLF-KLEV1 PIC X(20).                              
020110         05  FILLER               PIC X(01)  VALUE SPACE.                 
020300         05  RAD10-TENOTE1        PIC X(33).                              
020400         05  FILLER               PIC X(01)  VALUE SPACE.                 
020500     03  RAD10-2.                                                         
020610         05  RAD10-IDLEVTLF-KLEV2 PIC X(20).                              
020611         05  FILLER               PIC X(01)  VALUE SPACE.                 
020630         05  RAD10-TENOTE2        PIC X(33).                              
020640         05  FILLER               PIC X(01)  VALUE SPACE.                 
021100                                                                          
021101 01  RAD11.                                                               
021102     03  RAD11-1.                                                         
021103         05  FILLER              PIC X(01)  VALUE SPACE.                  
021104         05  RAD11-IDATTENT1     PIC ZZ.                                  
021105         05  FILLER              PIC X(01)  VALUE SPACE.                  
021106         05  RAD11-BELEV1        PIC X(35).                               
021107         05  FILLER              PIC X(17)  VALUE SPACE.                  
021108     03  RAD11-2.                                                         
021109         05  RAD11-IDATTENT2     PIC ZZ.                                  
021110         05  FILLER              PIC X(01)  VALUE SPACE.                  
021111         05  RAD11-BELEV2        PIC X(35).                               
021112         05  FILLER              PIC X(17)  VALUE SPACE.                  
021113                                                                          
021114 01  RAD12.                                                               
021115     03  RAD12-1.                                                         
021116         05  FILLER              PIC X(01)  VALUE SPACE.                  
021117         05  RAD12-IDMAIL1       PIC X(50).                               
021118         05  FILLER              PIC X(05)  VALUE SPACE.                  
021119     03  RAD12-2.                                                         
021120         05  RAD12-IDMAIL2       PIC X(50).                               
021121         05  FILLER              PIC X(05)  VALUE SPACE.                  
021122                                                                          
021123 01  RAD13.                                                               
021124     03  RAD13-1.                                                         
021125         05  FILLER               PIC X(01)  VALUE SPACE.                 
021126         05  RAD13-IDLEVTLF-KLEV1 PIC X(20).                              
021127         05  FILLER               PIC X(01)  VALUE SPACE.                 
021128         05  RAD13-TENOTE1        PIC X(33).                              
021129         05  FILLER               PIC X(01)  VALUE SPACE.                 
021130     03  RAD13-2.                                                         
021131         05  RAD13-IDLEVTLF-KLEV2 PIC X(20).                              
021132         05  FILLER               PIC X(01)  VALUE SPACE.                 
021133         05  RAD13-TENOTE2        PIC X(33).                              
021134         05  FILLER               PIC X(01)  VALUE SPACE.                 
025400     EJECT                                                                
025500 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
025600                                  'SORTWS-AREA-START  '.                  
025700     SKIP2                                                                
025800                                                                          
025900*01  AREA -COPY W21323      -PRE IN-                                      
026000 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
026100     EJECT                                                                
026200 PROCEDURE DIVISION.                                                      
026300 MAIN SECTION.                                                            
026400     SKIP2                                                                
026500                                                                          
026600     PERFORM A-INIT                                                       
026700                                                                          
026800     SORT SORTFIL ASCENDING KEY SORT-IDLEVNR                              
026900                  USING W21323                                            
027000                  OUTPUT PROCEDURE B-SORT-OUTPUT                          
027100                                                                          
027200     IF SORT-RETURN NOT = 0                                               
027300       MOVE SORT-RETURN TO SORT-RETURN-X                                  
027400       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
027500       DELIMITED BY SIZE INTO FELTEXT-STR                                 
027600       DISPLAY FELTEXT                                                    
027700       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
027800       PERFORM S99-ABEND                                                  
027900     ELSE                                                                 
028000       PERFORM Z-FINIT                                                    
028100                                                                          
028200       MOVE ZERO TO RETURN-CODE                                           
028300       GOBACK                                                             
028400     END-IF                                                               
028500                                                                          
028600     .                                                                    
028700     EJECT                                                                
028800 A-INIT SECTION.                                                          
028900                                                                          
029000     OPEN OUTPUT W21324                                                   
029100     SKIP2                                                                
029200     ACCEPT DAGENS-DATUM  FROM DATE                                       
029300     MOVE   DAGENS-DATUM    TO RUB-DATUM                                  
029400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029500     MOVE NEJ            TO FL-SKRIV-RADER                                
029600                            FL-RAD-DEL1-OK                                
029700     MOVE 99             TO ANTAL-RAD                                     
029800     MOVE ZERO           TO ANTAL-SID                                     
029900     .                                                                    
030000     EJECT                                                                
030100 B-SORT-OUTPUT SECTION.                                                   
030200     SKIP2                                                                
030300     PERFORM S31-SORT-RETURN                                              
030400     PERFORM UNTIL END-OF-SORTFIL                                         
030500       PERFORM C-LEVDATA                                                  
030600       PERFORM S31-SORT-RETURN                                            
030700     END-PERFORM                                                          
030800     .                                                                    
030900     EJECT                                                                
031000 C-LEVDATA SECTION.                                                       
031100                                                                          
031200*****MOVE SORTWS-AREA    TO UT-AREA                                       
031300                                                                          
031400     IF FL-SKRIV-RADER = JA                                               
031500        PERFORM S01-SKRIV-RADER                                           
031600        MOVE NEJ               TO FL-SKRIV-RADER                          
031700                                  FL-RAD-DEL1-OK                          
031800     ELSE                                                                 
031900        IF ANTAL-RAD =99                                                  
032000           MOVE 60             TO ANTAL-RAD                               
032100***        FÖRSTA POSTEN                                                  
032200        ELSE                                                              
032300           MOVE JA             TO FL-RAD-DEL1-OK                          
032400        END-IF                                                            
032500     END-IF                                                               
032600                                                                          
032700     PERFORM CA-EDIT-RAD                                                  
032800     .                                                                    
032900     EJECT                                                                
033000 CA-EDIT-RAD SECTION.                                                     
033100                                                                          
033200     IF FL-RAD-DEL1-OK = JA                                               
033300        MOVE JA                TO FL-SKRIV-RADER                          
033400        MOVE IN-IDLEVNR        TO RAD1-IDLEVNR2                           
033500        MOVE IN-BELEV          TO RAD2-BELEV2                             
033600        MOVE IN-ADLEV-RAD1     TO RAD3-ADLEV2                             
033700        MOVE IN-ADLEV-RAD2     TO RAD4-ADLEV2                             
033800        MOVE IN-ADLEV-ORT      TO RAD5-ADLEV-ORT2                         
033900        MOVE IN-ADLEVLND       TO RAD6-ADLEVLND2                          
034000        MOVE IN-IDLEVTLF       TO RAD7-IDLEVTLF2                          
034100     ELSE                                                                 
034200        MOVE IN-IDLEVNR        TO RAD1-IDLEVNR1                           
034300        MOVE IN-BELEV          TO RAD2-BELEV1                             
034400        MOVE IN-ADLEV-RAD1     TO RAD3-ADLEV1                             
034500        MOVE IN-ADLEV-RAD2     TO RAD4-ADLEV1                             
034600        MOVE IN-ADLEV-ORT      TO RAD5-ADLEV-ORT1                         
034700        MOVE IN-ADLEVLND       TO RAD6-ADLEVLND1                          
034800        MOVE IN-IDLEVTLF       TO RAD7-IDLEVTLF1                          
034900     END-IF                                                               
035000                                                                          
035010     IF FL-RAD-DEL1-OK = JA                                               
035020        MOVE IN-IDATTENT(1)      TO RAD8-IDATTENT2                        
035030        MOVE IN-ATT-BELEV(1)     TO RAD8-BELEV2                           
035031        MOVE IN-IDMAIL(1)        TO RAD9-IDMAIL2                          
035040        MOVE IN-IDLEVTLF-KLEV(1) TO RAD10-IDLEVTLF-KLEV2                  
035060        MOVE IN-TENOTE(1)        TO RAD10-TENOTE2                         
035070        MOVE IN-IDATTENT(2)      TO RAD11-IDATTENT2                       
035080        MOVE IN-ATT-BELEV(2)     TO RAD11-BELEV2                          
035090        MOVE IN-IDMAIL(2)        TO RAD12-IDMAIL2                         
035091        MOVE IN-IDLEVTLF-KLEV(2) TO RAD13-IDLEVTLF-KLEV2                  
035092        MOVE IN-TENOTE(2)        TO RAD13-TENOTE2                         
035093     ELSE                                                                 
035094        MOVE IN-IDATTENT(1)      TO RAD8-IDATTENT1                        
035095        MOVE IN-ATT-BELEV(1)     TO RAD8-BELEV1                           
035096        MOVE IN-IDMAIL(1)        TO RAD9-IDMAIL1                          
035097        MOVE IN-IDLEVTLF-KLEV(1) TO RAD10-IDLEVTLF-KLEV1                  
035098        MOVE IN-TENOTE(1)        TO RAD10-TENOTE1                         
035099        MOVE IN-IDATTENT(2)      TO RAD11-IDATTENT1                       
035100        MOVE IN-ATT-BELEV(2)     TO RAD11-BELEV1                          
035101        MOVE IN-IDMAIL(2)        TO RAD12-IDMAIL1                         
035102        MOVE IN-IDLEVTLF-KLEV(2) TO RAD13-IDLEVTLF-KLEV1                  
035103        MOVE IN-TENOTE(2)        TO RAD13-TENOTE1                         
035104     END-IF                                                               
035120     .                                                                    
035130                                                                          
035140     EJECT                                                                
035150 Z-FINIT SECTION.                                                         
035160                                                                          
035170     IF FL-RAD-DEL1-OK = NEJ                                              
037900        PERFORM ZA-NOLLA-2                                                
038000     END-IF                                                               
038100     PERFORM S01-SKRIV-RADER                                              
038200                                                                          
038300     CLOSE W21324                                                         
038400     SKIP2                                                                
038500     MOVE 'S' TO POSTSUM-OPKOD                                            
038600     CALL POSTSUM USING POSTSUM-PARM                                      
038700     .                                                                    
038800     EJECT                                                                
038900 ZA-NOLLA-2       SECTION.                                                
039000                                                                          
039100        MOVE SPACE             TO RAD1-IDLEVNR2                           
039200        MOVE SPACE             TO RAD2-BELEV2                             
039300        MOVE SPACE             TO RAD3-ADLEV2                             
039400        MOVE SPACE             TO RAD4-ADLEV2                             
039500        MOVE SPACE             TO RAD5-ADLEV-ORT2                         
039600        MOVE SPACE             TO RAD6-ADLEVLND2                          
039700        MOVE SPACE             TO RAD7-IDLEVTLF2                          
039710        MOVE ZERO              TO RAD8-IDATTENT1                          
039711        MOVE ZERO              TO RAD8-IDATTENT2                          
039720        MOVE SPACE             TO RAD8-BELEV1                             
039730        MOVE SPACE             TO RAD8-BELEV2                             
039800        MOVE SPACE             TO RAD9-IDMAIL1                            
039900        MOVE SPACE             TO RAD9-IDMAIL2                            
040000        MOVE SPACE             TO RAD10-IDLEVTLF-KLEV1                    
040010        MOVE SPACE             TO RAD10-IDLEVTLF-KLEV2                    
040100        MOVE SPACE             TO RAD10-TENOTE1                           
040200        MOVE SPACE             TO RAD10-TENOTE2                           
040210        MOVE ZERO              TO RAD11-IDATTENT1                         
040220        MOVE ZERO              TO RAD11-IDATTENT2                         
040300        MOVE SPACE             TO RAD11-BELEV1                            
040400        MOVE SPACE             TO RAD11-BELEV2                            
040500        MOVE SPACE             TO RAD12-IDMAIL1                           
040600        MOVE SPACE             TO RAD12-IDMAIL2                           
040700        MOVE SPACE             TO RAD13-IDLEVTLF-KLEV1                    
040710        MOVE SPACE             TO RAD13-IDLEVTLF-KLEV2                    
040720        MOVE SPACE             TO RAD13-TENOTE1                           
040730        MOVE SPACE             TO RAD13-TENOTE2                           
040800     .                                                                    
040900     EJECT                                                                
041000 S01-SKRIV-RADER  SECTION.                                                
041100                                                                          
041200     IF ANTAL-RAD   > 55                                                  
041300        PERFORM S02-SKRIV-RUBRIK                                          
041400     END-IF                                                               
041500     MOVE RAD1   TO UT-AREA                                               
041600     PERFORM S11-SKRIV-W21324                                             
041700     MOVE RAD2   TO UT-AREA                                               
041800     PERFORM S11-SKRIV-W21324                                             
041900     MOVE RAD3   TO UT-AREA                                               
042000     PERFORM S11-SKRIV-W21324                                             
042100     MOVE RAD4   TO UT-AREA                                               
042200     PERFORM S11-SKRIV-W21324                                             
042300     MOVE RAD5   TO UT-AREA                                               
042400     PERFORM S11-SKRIV-W21324                                             
042500     MOVE RAD6   TO UT-AREA                                               
042600     PERFORM S11-SKRIV-W21324                                             
042700     MOVE RAD7   TO UT-AREA                                               
042800     PERFORM S11-SKRIV-W21324                                             
042810     MOVE RAD8   TO UT-AREA                                               
042900     PERFORM S11-SKRIV-W21324                                             
043000     MOVE RAD9   TO UT-AREA                                               
043100     PERFORM S11-SKRIV-W21324                                             
043200     MOVE RAD10  TO UT-AREA                                               
043300     PERFORM S11-SKRIV-W21324                                             
043400     MOVE RAD11  TO UT-AREA                                               
043500     PERFORM S11-SKRIV-W21324                                             
043600     MOVE RAD12  TO UT-AREA                                               
043700     PERFORM S11-SKRIV-W21324                                             
043800     MOVE RAD13  TO UT-AREA                                               
043900     PERFORM S11-SKRIV-W21324                                             
044000     MOVE SPACE  TO UT-AREA                                               
044100     PERFORM S11-SKRIV-W21324                                             
044200     ADD 14      TO ANTAL-RAD                                             
044300     .                                                                    
044400     EJECT                                                                
044500 S02-SKRIV-RUBRIK SECTION.                                                
044600                                                                          
044700     ADD +1              TO ANTAL-SID                                     
044800     MOVE ZERO           TO ANTAL-RAD                                     
044900     MOVE RAD1-IDLEVNR1  TO RUB-IDLEVNR                                   
045000     MOVE ANTAL-SID      TO RUB-SIDA                                      
045100     MOVE RUBRIK         TO UT-AREA                                       
045200     PERFORM S11-SKRIV-W21324                                             
045300     MOVE SPACE          TO UT-AREA                                       
045400     PERFORM S11-SKRIV-W21324                                             
045500     .                                                                    
045600     EJECT                                                                
045700 S11-SKRIV-W21324 SECTION.                                                
045800                                                                          
045900     WRITE UT-POST FROM UT-AREA                                           
046000                                                                          
046100     MOVE 'RAD'      TO POSTSUM-TRANSTYP                                  
046200     MOVE 'W21324'   TO POSTSUM-FDNAMN                                    
046300     MOVE 'W21323D2' TO POSTSUM-DDNAMN2                                   
046400     CALL POSTSUM USING POSTSUM-PARM                                      
046500     .                                                                    
046600     EJECT                                                                
046700 S31-SORT-RETURN  SECTION.                                                
046800                                                                          
046900     RETURN SORTFIL INTO IN-AREA                                          
047000     AT END                                                               
047100         SET END-OF-SORTFIL TO TRUE                                       
047200     .                                                                    
047300     EJECT                                                                
047400 S99-ABEND SECTION.                                                       
047500                                                                          
047600     SKIP2                                                                
047700     MOVE 'S' TO POSTSUM-OPKOD                                            
047800     CALL POSTSUM USING POSTSUM-PARM                                      
047900     CALL ABEND USING RKOD-ABEND                                          
048000     .                                                                    
