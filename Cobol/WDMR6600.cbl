000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WDMR6600.                                                
000400*AUTHOR.         KARIN OLSSON.                                            
000500*DATE-WRITTEN.   92/03/24.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPA LADDFIL FÖR DATA MANAGER                                   
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- PSB                                                        
002600     SELECT WDMR64                     ASSIGN TO WDMR66D1.                
002700     SKIP2                                                                
002800*          --- DMRLADD-DATA                                               
002900     SELECT WDMR66                     ASSIGN TO WDMR66D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  WDMR64                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900 01  FILLER                   PIC X(26).                                  
004000     SKIP3                                                                
004100 FD  WDMR66                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500 01  PSB-POST                 PIC X(80).                                  
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'WDMR6600'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  INPUTS                      PIC X       VALUE 'I'.                   
005500 77  OUTPUTS                     PIC X       VALUE 'O'.                   
005600 77  UPDATES                     PIC X       VALUE 'U'.                   
005700 77  TRANSREF                    PIC X       VALUE 'T'.                   
005800 77  W-STR                       PIC X(200).                              
005900 77  W-CAT-WORD                  PIC X(10).                               
006000 77  END-PUNKT                   PIC X       VALUE 'N'.                   
006100 77  SKRIVIT-INPUTS              PIC X       VALUE 'N'.                   
006200 77  SKRIVIT-OUTPUTS             PIC X       VALUE 'N'.                   
006300 77  SKRIVIT-UPDATES             PIC X       VALUE 'N'.                   
006400 77  SKRIVIT-TRANSREF            PIC X       VALUE 'N'.                   
006500 77  PSB-CAT                     PIC X(10)                                
006600                                     VALUE '''PSB'''.                     
006700                                                                          
006800 77  SPAR-PSB                    PIC X(12)   VALUE SPACE.                 
006900     EJECT                                                                
007000* EOF FLAGGOR                                                             
007100                                                                          
007200 77  WDMR64-EOF-SW               PIC X       VALUE 'N'.                   
007300     88  END-OF-WDMR64                       VALUE 'J'.                   
007400     EJECT                                                                
007500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008000     SKIP3                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200*                                                                         
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008500                                                                          
008600*    --- PARAMETRAR TILL ABEND                                            
008700                                                                          
008800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009000     SKIP2                                                                
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400     EJECT                                                                
009500*    --- ARBETSAREOR FÖR REDIGERING AV UTFILER                            
009600 01  REPLACE-COMMAND.                                                     
009700     03                          PIC X(08) VALUE 'REPLACE'.               
009800     03 REPL-MEMBER              PIC X(50).                               
009900     03                          PIC X(01) VALUE '.'.                     
010000     SKIP2                                                                
010100 01  REMOVE-COMMAND.                                                      
010200     03                          PIC X(08) VALUE 'REMOVE'.                
010300     03 REM-MEMBER               PIC X(50).                               
010400     03                          PIC X(01) VALUE '.'.                     
010500     SKIP2                                                                
010600 01  RUBRIKER.                                                            
010700     03 RUB-MODULE               PIC X(60) VALUE 'MODULE'.                
010800     03 RUB-CATALOG              PIC X(60) VALUE 'CATALOG'.               
010900     03 RUB-INPUTS               PIC X(60) VALUE 'INPUTS'.                
011000     03 RUB-OUTPUTS              PIC X(60) VALUE 'OUTPUTS'.               
011100     03 RUB-UPDATES              PIC X(60) VALUE 'UPDATES'.               
011200     03 RUB-TRANSREF             PIC X(60) VALUE 'CALLS'.                 
011300     SKIP2                                                                
011400 01  CAT-NAMES.                                                           
011500     03                          PIC X(04) VALUE SPACE.                   
011600     03 CAT-NAME-AREA            PIC X(70).                               
011700     SKIP2                                                                
011800 01  CONT-POST.                                                           
011900     03                          PIC X(04) VALUE SPACE.                   
012000     03 CONT-KOMMA               PIC X(1).                                
012100     03 CONT-MBR                 PIC X(50).                               
012200     SKIP2                                                                
012300*    --- AREA FÖR ATT ÄNDRA TRANSAKTION TILL PSB                          
012400 01  TRANSFER-NAMN               PIC X(10).                               
012500     SKIP2                                                                
012600*    --- PARAMETRAR TILL POSTSUM                                          
012700*                                                                         
012800*01  -COPY W0005   -PRE  POSTSUM-                                         
012900     EJECT                                                                
013000 01  PSB-AREA-START              PIC X(24)   VALUE                        
013100                                 'PSB-AREA-START  '.                      
013200                                                                          
013300 01  IN-PSB-AREA.                                                         
013400     03 PSB-STYR                 PIC X(01).                               
013500     03 PSB-NAMN                 PIC X(12).                               
013600     03 PSB-REF                  PIC X(1).                                
013700     03 PSB-DB                   PIC X(12).                               
013800     EJECT                                                                
013900 01  UTPSB-AREA-START            PIC X(24)   VALUE                        
014000                                 'UTPSB-AREA-START  '.                    
014100 01  UT-PSB-AREA                 PIC X(80).                               
014200                                                                          
014300     EJECT                                                                
014400 PROCEDURE DIVISION.                                                      
014500     SKIP2                                                                
014600 STYR SECTION.                                                            
014700                                                                          
014800     PERFORM A-INIT                                                       
014900     PERFORM B-SKAPA-PSB                                                  
015000                                                                          
015100     PERFORM Z-FINIT                                                      
015200                                                                          
015300     MOVE ZERO TO RETURN-CODE                                             
015400     GOBACK                                                               
015500     .                                                                    
015600     EJECT                                                                
015700 A-INIT SECTION.                                                          
015800                                                                          
015900     OPEN INPUT WDMR64                                                    
016000                                                                          
016100     OPEN OUTPUT WDMR66                                                   
016200     SKIP2                                                                
016300     ACCEPT DAGENS-DATUM  FROM DATE                                       
016400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016500     .                                                                    
016600     EJECT                                                                
016700 B-SKAPA-PSB SECTION.                                                     
016800                                                                          
016900     PERFORM S01-LAES-WDMR64                                              
017000     PERFORM UNTIL END-OF-WDMR64                                          
017100       IF SPAR-PSB NOT = PSB-NAMN                                         
017200         MOVE NEJ TO SKRIVIT-INPUTS                                       
017300                     SKRIVIT-OUTPUTS                                      
017400                     SKRIVIT-UPDATES                                      
017500                     SKRIVIT-TRANSREF                                     
017600         IF END-PUNKT = NEJ                                               
017700           MOVE JA TO END-PUNKT                                           
017800         ELSE                                                             
017900           MOVE '.' TO UT-PSB-AREA                                        
018000           PERFORM S11-SKRIV-WDMR66                                       
018100         END-IF                                                           
018200         MOVE PSB-NAMN TO SPAR-PSB                                        
018300         IF PSB-STYR NOT = 'D'                                            
018400           PERFORM BA-FIRST-NEW-RCD                                       
018500           PERFORM BB-NEXT-NEW-RCD                                        
018600         ELSE                                                             
018700           PERFORM BC-DELETE-RCD                                          
018800         END-IF                                                           
018900       ELSE                                                               
019000         PERFORM BB-NEXT-NEW-RCD                                          
019100       END-IF                                                             
019200                                                                          
019300       PERFORM S01-LAES-WDMR64                                            
019400     END-PERFORM                                                          
019500     MOVE '.' TO UT-PSB-AREA                                              
019600     PERFORM S11-SKRIV-WDMR66                                             
019700     .                                                                    
019800     EJECT                                                                
019900 BA-FIRST-NEW-RCD SECTION.                                                
020000                                                                          
020100     MOVE PSB-NAMN TO REPL-MEMBER                                         
020200     MOVE REPLACE-COMMAND TO UT-PSB-AREA                                  
020300     PERFORM S11-SKRIV-WDMR66                                             
020400                                                                          
020500     MOVE RUB-MODULE      TO UT-PSB-AREA                                  
020600     PERFORM S11-SKRIV-WDMR66                                             
020700                                                                          
020800     MOVE SPACE           TO UT-PSB-AREA                                  
020900     PERFORM S11-SKRIV-WDMR66                                             
021000     MOVE RUB-CATALOG     TO UT-PSB-AREA                                  
021100     PERFORM S11-SKRIV-WDMR66                                             
021200                                                                          
021300     MOVE PSB-CAT TO CAT-NAME-AREA                                        
021400     MOVE CAT-NAMES TO UT-PSB-AREA                                        
021500     PERFORM S11-SKRIV-WDMR66                                             
021600     .                                                                    
021700     EJECT                                                                
021800 BB-NEXT-NEW-RCD SECTION.                                                 
021900                                                                          
022000     EVALUATE TRUE                                                        
022100       WHEN PSB-REF = INPUTS                                              
022200         PERFORM BBA-SKRIV-INPUTS                                         
022300       WHEN PSB-REF = OUTPUTS                                             
022400         PERFORM BBB-SKRIV-OUTPUTS                                        
022500       WHEN PSB-REF = UPDATES                                             
022600         PERFORM BBC-SKRIV-UPDATES                                        
022700       WHEN PSB-REF = TRANSREF                                            
022800         PERFORM BBD-SKRIV-TRANSREF                                       
022900       WHEN OTHER                                                         
023000         STRING 'OKÄND REF-TYP' PSB-REF                                   
023100           DELIMITED BY SIZE INTO FELTEXT-STR                             
023200           DISPLAY FELTEXT                                                
023300         PERFORM S99-ABEND                                                
023400     END-EVALUATE                                                         
023500     .                                                                    
023600     EJECT                                                                
023700 BBA-SKRIV-INPUTS SECTION.                                                
023800                                                                          
023900     IF SKRIVIT-INPUTS = NEJ                                              
024000       MOVE SPACE           TO UT-PSB-AREA                                
024100       PERFORM S11-SKRIV-WDMR66                                           
024200       MOVE RUB-INPUTS  TO UT-PSB-AREA                                    
024300       PERFORM S11-SKRIV-WDMR66                                           
024400       MOVE JA TO SKRIVIT-INPUTS                                          
024500       MOVE SPACE TO CONT-KOMMA                                           
024600     ELSE                                                                 
024700       MOVE ','   TO CONT-KOMMA                                           
024800     END-IF                                                               
024900                                                                          
025000     MOVE PSB-DB TO CONT-MBR                                              
025100     MOVE CONT-POST TO UT-PSB-AREA                                        
025200     PERFORM S11-SKRIV-WDMR66                                             
025300     .                                                                    
025400     EJECT                                                                
025500 BBB-SKRIV-OUTPUTS SECTION.                                               
025600                                                                          
025700     IF SKRIVIT-OUTPUTS = NEJ                                             
025800       MOVE SPACE           TO UT-PSB-AREA                                
025900       PERFORM S11-SKRIV-WDMR66                                           
026000       MOVE RUB-OUTPUTS TO UT-PSB-AREA                                    
026100       PERFORM S11-SKRIV-WDMR66                                           
026200       MOVE JA TO SKRIVIT-OUTPUTS                                         
026300       MOVE SPACE TO CONT-KOMMA                                           
026400     ELSE                                                                 
026500       MOVE ','   TO CONT-KOMMA                                           
026600     END-IF                                                               
026700                                                                          
026800     MOVE PSB-DB TO CONT-MBR                                              
026900     MOVE CONT-POST TO UT-PSB-AREA                                        
027000     PERFORM S11-SKRIV-WDMR66                                             
027100     .                                                                    
027200     EJECT                                                                
027300 BBC-SKRIV-UPDATES SECTION.                                               
027400                                                                          
027500     IF SKRIVIT-UPDATES = NEJ                                             
027600       MOVE SPACE           TO UT-PSB-AREA                                
027700       PERFORM S11-SKRIV-WDMR66                                           
027800       MOVE RUB-UPDATES TO UT-PSB-AREA                                    
027900       PERFORM S11-SKRIV-WDMR66                                           
028000       MOVE JA TO SKRIVIT-UPDATES                                         
028100       MOVE SPACE TO CONT-KOMMA                                           
028200     ELSE                                                                 
028300       MOVE ','   TO CONT-KOMMA                                           
028400     END-IF                                                               
028500                                                                          
028600     MOVE PSB-DB TO CONT-MBR                                              
028700     MOVE CONT-POST TO UT-PSB-AREA                                        
028800     PERFORM S11-SKRIV-WDMR66                                             
028900     .                                                                    
029000     EJECT                                                                
029100 BBD-SKRIV-TRANSREF  SECTION.                                             
029200                                                                          
029300     IF SKRIVIT-TRANSREF = NEJ                                            
029400       MOVE SPACE           TO UT-PSB-AREA                                
029500       PERFORM S11-SKRIV-WDMR66                                           
029600       MOVE RUB-TRANSREF TO UT-PSB-AREA                                   
029700       PERFORM S11-SKRIV-WDMR66                                           
029800       MOVE JA TO SKRIVIT-TRANSREF                                        
029900       MOVE SPACE TO CONT-KOMMA                                           
030000     ELSE                                                                 
030100       MOVE ','   TO CONT-KOMMA                                           
030200     END-IF                                                               
030300                                                                          
030400*    -- OMVANDLA TRANSAKTIONSNAMN TILL PSB-NAMN                           
030500     MOVE PSB-DB TO TRANSFER-NAMN                                         
030600     IF TRANSFER-NAMN(3:1) = 'T'                                          
030700       MOVE '0' TO  TRANSFER-NAMN(3:1)                                    
030710     END-IF                                                               
030720     MOVE '-PSB' TO TRANSFER-NAMN(7:)                                     
030900     MOVE TRANSFER-NAMN TO CONT-MBR                                       
031000                                                                          
031100     MOVE CONT-POST TO UT-PSB-AREA                                        
031200     PERFORM S11-SKRIV-WDMR66                                             
031300     .                                                                    
031400     EJECT                                                                
031500 BC-DELETE-RCD SECTION.                                                   
031600                                                                          
031700     MOVE PSB-NAMN TO REM-MEMBER                                          
031800     MOVE REMOVE-COMMAND       TO UT-PSB-AREA                             
031900     PERFORM S11-SKRIV-WDMR66                                             
032000     MOVE NEJ TO END-PUNKT                                                
032100     .                                                                    
032200     EJECT                                                                
032300 Z-FINIT SECTION.                                                         
032400                                                                          
032500     CLOSE WDMR64 WDMR66                                                  
032600     SKIP2                                                                
032700     MOVE 'S' TO POSTSUM-OPKOD                                            
032800     CALL POSTSUM USING POSTSUM-PARM                                      
032900     .                                                                    
033000     EJECT                                                                
033100 S01-LAES-WDMR64  SECTION.                                                
033200     SKIP2                                                                
033300     READ WDMR64 INTO IN-PSB-AREA                                         
033400     AT END                                                               
033500        SET END-OF-WDMR64 TO TRUE                                         
033600                                                                          
033700     NOT AT END                                                           
033800        MOVE 'WDMR64'    TO POSTSUM-FDNAMN                                
033900        MOVE 'WDMR66D1'  TO POSTSUM-DDNAMN2                               
034000        CALL POSTSUM USING POSTSUM-PARM                                   
034100     END-READ                                                             
034200     .                                                                    
034300     EJECT                                                                
034400 S11-SKRIV-WDMR66 SECTION.                                                
034500     SKIP2                                                                
034600     WRITE PSB-POST FROM UT-PSB-AREA                                      
034700                                                                          
034800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
034900     MOVE 'WDMR66'   TO POSTSUM-FDNAMN                                    
035000     MOVE 'WDMR66D2' TO POSTSUM-DDNAMN2                                   
035100     CALL POSTSUM USING POSTSUM-PARM                                      
035200     .                                                                    
035300     EJECT                                                                
035400 S99-ABEND SECTION.                                                       
035500                                                                          
035600     SKIP2                                                                
035700     MOVE 'S' TO POSTSUM-OPKOD                                            
035800     CALL POSTSUM USING POSTSUM-PARM                                      
035900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
036000     .                                                                    
