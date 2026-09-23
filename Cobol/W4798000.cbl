000100 ID DIVISION.                                                             
000200     SKIP3                                                                
000300 PROGRAM-ID.             W4798000.                                        
000400 AUTHOR.                 ROYNA LUND.                                      
000500 DATE-WRITTEN.           MAR 1988.                                        
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER EN "EFR-FIL" OCH SKAPAR                         
001100*        EN FIL MED SUMMA FÖRSÄLJNNG PER MARKNAD OCH PRODUKTKOD           
001200*        (NETTOPRIS).                                                     
001300*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP3                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800     SKIP2                                                                
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*    ------- INFIL:                                                       
002200                                                                          
002300     SELECT  W4795B        ASSIGN  W47980D1.                              
002400                                                                          
002500*    ------  UTFIL.                                                       
002600                                                                          
002700     SELECT  W47980        ASSIGN  W47980D2.                              
002800                                                                          
002900*    ------- SORTFIL:                                                     
003000                                                                          
003100     SELECT  SORTER        ASSIGN  W47980DS.                              
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600                                                                          
003700 FD  W4795B                                                               
003800     LABEL RECORD STANDARD                                                
003900     RECORDING  F                                                         
004000     BLOCK CONTAINS 0.                                                    
004100                                                                          
004200*    -COPY W47905B          -L.                                           
004400                                                                          
004500                                                                          
004600 FD  W47980                                                               
004700     LABEL RECORD STANDARD                                                
004800     RECORDING  V                                                         
004900     BLOCK CONTAINS 0.                                                    
005000                                                                          
005100 01  UT-POST  -COPY W479080 -L.                                           
005900     EJECT                                                                
006000 SD  SORTER                                                               
006100     RECORDING  F.                                                        
006200                                                                          
006300 01  SORT-POST.                                                           
006400     03  SORT-IDARTNR        PIC S9(7)      COMP-3.                       
006500     03  SORT-KDMARK         PIC S9(3)      COMP-3.                       
006600     03  SORT-KDPRODSL       PIC S9(3)      COMP-3.                       
006700     03  SORT-SUARTNTO       PIC S9(9)V9(2) COMP-3.                       
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
007000     SKIP2                                                                
007001                                                                          
007010*    -- CHECKED BY WY2000                                                 
007100*    ---- GENERERAT PROGRAM-NAMN                                          
007200                                                                          
007300 77  PROGRAM-NAMN            PIC X(8)        VALUE 'W4798000'.            
007400     SKIP3                                                                
007500*    ------- KONSTANTER                                                   
007600                                                                          
007700 77  JA                      PIC X           VALUE 'J'.                   
007800 77  NEJ                     PIC X           VALUE 'N'.                   
007900     SKIP3                                                                
008000*    ------- END-OF-FILE SWITCHAR                                         
008100                                                                          
008200 77  W4795B-EOF              PIC X           VALUE 'N'.                   
008300     88  SLUT-W4795B                         VALUE 'J'.                   
008400 77  SORTER-EOF              PIC X           VALUE 'N'.                   
008500     88  SLUT-SORTER                         VALUE 'J'.                   
008600     SKIP3                                                                
008700*    ------- ÖVRIGA VARIABLER                                             
008800                                                                          
008900 01  WS-AAVV                 PIC S9(5).                                   
009000 01  WS-AAVV-R               REDEFINES WS-AAVV.                           
009100     03  WS-FORE-AA          PIC 9.                                       
009200     03  WS-AA               PIC 9(2).                                    
009300     03  WS-VV               PIC 9(2).                                    
009400     EJECT                                                                
009500*    ------- SUBPROGRAM                                                   
009600                                                                          
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800     03 ABEND                PIC X(8)    VALUE 'ABEND   '.                
009900     03 POSTSUM              PIC X(8)    VALUE 'POSTSUM '.                
010000     03 DATKORT              PIC X(8)    VALUE 'DATKORT '.                
010100     03 WKPSKONV             PIC X(8)    VALUE 'WKPSKONV'.                
010200     03 W510MARK             PIC X(8)    VALUE 'W510MARK'.                
010400     SKIP3                                                                
010500*    ------- PARAMETRAR TILL ABEND                                        
010600                                                                          
010700 01  RETURKODER.                                                          
010800     03  RKOD                 PIC S9(4)  COMP SYNC VALUE ZERO.            
010900     03  RKOD-ABEND-UTAN-DUMP PIC S9(4)  COMP SYNC VALUE +16.             
011000     03  RKOD-ABEND-MED-DUMP  PIC S9(4)  COMP SYNC VALUE +1000.           
011100     EJECT                                                                
011200*    ------- PARAMETRAR TILL POSTSUM                                      
011300                                                                          
011400*01  -COPY W0005       -PRE POSTSUM-.                                     
011600     EJECT                                                                
011700*    ------- PARAMETRAR TILL DATUMKORT                                    
011800                                                                          
011900 01  DATUMKORT-ID            PIC X(6)   VALUE 'WDATUM'.                   
012000                                                                          
012100*    -COPY WDATKORT                                                       
012300     EJECT                                                                
012400*    ------- PARAMETRAR TILL WKPSKONV                                     
012500                                                                          
012600*    -COPY WKPSAREA                                                       
012800     EJECT                                                                
012900*    ------- PARAMETRAR TILL W510MARK                                     
013000                                                                          
013100*    -COPY W510MARK                                                       
013300     EJECT                                                                
013900*    ------- AREA FÖR IN-POSTER                                           
014000                                                                          
014100 01  FILLER                  PIC X(24) VALUE 'WS-INAREA-START'.           
014200     SKIP3                                                                
014300 01  WS-INAREA.                                                           
014400*    03  AREA  -COPY W47905B    -PRE IN-.                                 
014600     EJECT                                                                
014700 01  FILLER                  PIC X(24) VALUE 'WS-UTAREA-START'.           
014800     SKIP3                                                                
014900 01  WS-UTAREA.                                                           
015300*    03 RW4-AREA   -COPY W479080    -PRE  UT-.                            
015500     EJECT                                                                
015600 01  FILLER                  PIC X(24) VALUE 'WS-SORTAREA-START'.         
015700     SKIP3                                                                
015800 01  WS-SORTAREA.                                                         
015900     03  WS-IDARTNR          PIC S9(7)      COMP-3.                       
016000     03  WS-KDMARK           PIC S9(3)      COMP-3.                       
016100     03  WS-KDPRODSL         PIC S9(3)      COMP-3.                       
016200     03  WS-SUARTNTO         PIC S9(9)V9(2) COMP-3.                       
016300     EJECT                                                                
016400 PROCEDURE DIVISION.                                                      
016500     SKIP2                                                                
016600                                                                          
016700     PERFORM A-INITIERA                                                   
016800                                                                          
016900     SORT SORTER      ASCENDING                                           
017000                      SORT-KDMARK                                         
017100                      SORT-KDPRODSL                                       
017200                                                                          
017300     INPUT PROCEDURE  B-SELEKTERA-POSTER-FALT                             
017400     OUTPUT PROCEDURE C-SUMMERA-NETTOPRIS                                 
017500                                                                          
017600     IF SORT-RETURN > ZERO                                                
017700         DISPLAY 'W4798000-FEL VID SORTERING'                             
017800         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
017900     ELSE                                                                 
018000         PERFORM Z-AVSLUTA                                                
018100         MOVE    ZERO TO RETURN-CODE                                      
018200         GOBACK                                                           
018300     END-IF                                                               
018400     .                                                                    
018500     EJECT                                                                
018600 A-INITIERA SECTION.                                                      
018700     SKIP2                                                                
018800     OPEN  INPUT    W4795B                                                
018900           OUTPUT   W47980                                                
019000                                                                          
019100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
019200                                                                          
019300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
019400                                                                          
019500     MOVE ZERO      TO WS-FORE-AA                                         
019600     MOVE D-AAR     TO WS-AA                                              
019700     MOVE D-VECKA   TO WS-VV                                              
019800                                                                          
019900     MOVE 'RW4'     TO UT-IDPTYP                                          
020000     MOVE WS-AAVV   TO UT-TIAAVV                                          
020100     MOVE ZERO      TO UT-KDWRTYP                                         
020200                       UT-IDDC                                            
020300                       UT-IDDISTR                                         
020400     .                                                                    
020500     EJECT                                                                
020600 B-SELEKTERA-POSTER-FALT SECTION.                                         
020700     SKIP2                                                                
020800     PERFORM S01-LAS-W4795B                                               
020900                                                                          
021000     PERFORM UNTIL SLUT-W4795B                                            
021100         MOVE IN-IDARTNR TO WS-IDARTNR                                    
021700                                                                          
021800         MOVE IN-IDLKTO           TO KPS-IDLKTO                           
021900         MOVE 001                 TO KPS-KDCALL                           
022000         CALL WKPSKONV USING KPS-WKPSAREA                                 
022100         MOVE KPS-KDPRODSL        TO WS-KDPRODSL                          
022110                                                                          
022120         MOVE ZERO                TO MARK-KDCALL                          
022130         MOVE IN-IDDISTR          TO MARK-IDDISTR                         
022140         CALL W510MARK USING      MARK-W510MARK                           
022150         MOVE MARK-KDMARK-BUDG TO WS-KDMARK                               
022200                                                                          
022300         COMPUTE WS-SUARTNTO = IN-PRARTNTO *                              
022400                 (IN-KVEFRS-PACK + IN-KVEFRS-OPACK)                       
022500                                                                          
022600         PERFORM S04-SKRIV-SORTER                                         
022700         PERFORM S01-LAS-W4795B                                           
022800     END-PERFORM                                                          
022900     .                                                                    
023000     EJECT                                                                
023100 C-SUMMERA-NETTOPRIS SECTION.                                             
023200     SKIP2                                                                
023300     PERFORM S02-LAS-SORTER                                               
023400                                                                          
023500     PERFORM UNTIL SLUT-SORTER                                            
023600                                                                          
023700         MOVE ZERO        TO UT-SUARTNTO                                  
023800                                                                          
023900         MOVE WS-KDMARK   TO UT-KDMARK                                    
024000         MOVE WS-KDPRODSL TO UT-KDPRODSL                                  
024100                                                                          
025200         PERFORM UNTIL SLUT-SORTER OR                                     
025300                       WS-KDMARK   NOT = UT-KDMARK  OR                    
025400                       WS-KDPRODSL NOT = UT-KDPRODSL                      
025500                                                                          
025600             COMPUTE UT-SUARTNTO = UT-SUARTNTO + WS-SUARTNTO              
025700                                                                          
025800             PERFORM S02-LAS-SORTER                                       
025900         END-PERFORM                                                      
026000         PERFORM S03-SKRIV-W47980                                         
026100     END-PERFORM                                                          
026200     .                                                                    
026300     EJECT                                                                
026400 S01-LAS-W4795B SECTION.                                                  
026500     SKIP2                                                                
026600     READ   W4795B INTO WS-INAREA                                         
026700                   AT END MOVE JA TO W4795B-EOF                           
026800     END-READ                                                             
026900                                                                          
027000     IF  NOT SLUT-W4795B                                                  
027100         MOVE 'W4795B'   TO POSTSUM-FDNAMN                                
027200         MOVE 'W47980D1' TO POSTSUM-DDNAMN2                               
027300         MOVE SPACE      TO POSTSUM-TRANSTYP                              
027400         CALL POSTSUM USING POSTSUM-PARM                                  
027500     END-IF                                                               
027600     .                                                                    
027700     EJECT                                                                
027800 S02-LAS-SORTER SECTION.                                                  
027900     SKIP2                                                                
028000     RETURN SORTER INTO WS-SORTAREA                                       
028100                   AT END MOVE JA TO SORTER-EOF                           
028200     END-RETURN                                                           
028300     .                                                                    
028400     EJECT                                                                
028500 S03-SKRIV-W47980 SECTION.                                                
028600     SKIP2                                                                
028700     WRITE  UT-POST FROM WS-UTAREA                                        
028800                                                                          
028900     MOVE 'W47980'   TO POSTSUM-FDNAMN                                    
029000     MOVE 'W47980D2' TO POSTSUM-DDNAMN2                                   
029100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
029200     CALL POSTSUM USING POSTSUM-PARM                                      
029300     .                                                                    
029400     EJECT                                                                
029500 S04-SKRIV-SORTER SECTION.                                                
029600     SKIP2                                                                
029700     RELEASE SORT-POST FROM WS-SORTAREA                                   
029800                                                                          
029900     MOVE 'SORTER'   TO POSTSUM-FDNAMN                                    
030000     MOVE 'W47980DS' TO POSTSUM-DDNAMN2                                   
030100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
030200     CALL POSTSUM USING POSTSUM-PARM                                      
030300     .                                                                    
030400     EJECT                                                                
030500 Z-AVSLUTA SECTION.                                                       
030600     SKIP2                                                                
030700                                                                          
030800     CLOSE  W4795B                                                        
030900            W47980                                                        
031000                                                                          
031100*    -----  SKRIV UT ANTAL LÄSTA OCH SKRIVNA POSTER  -----                
031200     MOVE 'S' TO POSTSUM-OPKOD                                            
031300     CALL POSTSUM USING POSTSUM-PARM                                      
031400     .                                                                    
