000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4765700.                                                
000300 AUTHOR.         ELEONOR ÖSTRÖM.                                          
000400 DATE-WRITTEN.   04/10/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        THIS PROGRAM READS FILE W47654 (SALES STATISTIC) AND             
001000*        CHECK IF KDPRTYP = T WHICH MEANS THAT THE ORDERLINE IS           
001100*        INVOICED WITH COST * 2 OR COST * 4. A REPORT IS SEND TO          
001200*        PRICEDEP AT VCC.                                                 
001300*                                                                         
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900*    REMARKS:                                                             
002000*        ETRACKER NR 1424741                                              
002100*        ETRACKER NR 5944661 , SKICKA LISTAN TILL DAP                     
002200*          OCH TA BORT HEAD FILEN. SÄNDNINGEN TILL DAP SKER               
002300*          I JCL W476J057                                                 
002400*                                                                         
002500*                                                                         
002600*                                                                         
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     SKIP2                                                                
003600*          --- SALES STATISTIC INVOIC                                     
003700     SELECT W47654                     ASSIGN TO W47657D1.                
003800     SKIP2                                                                
003900*          --- REPORT FILE                                                
004000     SELECT W47657                     ASSIGN TO W47657D2.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W47654                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY W330099     -L.                                                
005100     SKIP3                                                                
005200 FD  W47657                                                               
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600*01  POST -COPY W47657 -PRE  REP-  -L.                                    
005700     SKIP3                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200 77  IDPGM                       PIC X(8)    VALUE 'W4765700'.            
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500                                                                          
006600 77  W47654-EOF-SW               PIC X       VALUE 'N'.                   
006700     88  END-OF-W47654                       VALUE 'J'.                   
006800     EJECT                                                                
006900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007000 01  FILLER REDEFINES DAGENS-DATUM.                                       
007100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007400     EJECT                                                                
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700       03  ABEND                   PIC X(8)    VALUE 'ABEND'.             
007800       03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.           
007900       03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.            
008000       03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.           
008100     SKIP2                                                                
008200*    --- PARAMETRAR TILL ABEND                                            
008300                                                                          
008400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008700     SKIP2                                                                
008800 01  FELTEXT.                                                             
008900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600 01  IN01-AREA-START             PIC X(24)   VALUE                        
009700                                 'IN01-AREA-START  '.                     
009800     SKIP2                                                                
009900                                                                          
010000*01  AREA -COPY W330099    -PRE IN01-                                     
010100     EJECT                                                                
010200 01  REP-AREA-START              PIC X(24)   VALUE                        
010300                                 'REP-AREA-START  '.                      
010400     SKIP2                                                                
010500*01  AREA -COPY W47657     -PRE REP-                                      
010600     EJECT                                                                
010700 01  HEAD-AREA-START              PIC X(24)   VALUE                       
010800                                 'HEAD-AREA-START '.                      
010900 01  HEAD-AREA.                                                           
011000     03  FILLER                   PIC X(80).                              
011100     EJECT                                                                
011200 01  W-REPORT-LAYOUT.                                                     
011300     03 T-RUB01.                                                          
011400       05 FILLER           PIC X(48) VALUE                                
011500          'PARTNUMBER  DISTRICT      NETPRICE    INVOICEDAT'.             
011600     03 T-RUB02.                                                          
011700       05 FILLER           PIC X(80) VALUE SPACE.                         
011800     03 T-RUB03.                                                          
011900       05 FILLER           PIC X(80) VALUE                                
012000          'ORDERLINES INVOICED AT COST * 2 AND 4'.                        
012100     03 TOMRAD.                                                           
012200       05 FILLER           PIC X(80) VALUE SPACE.                         
012300     03 T-RAD01.                                                          
012400       05 FILLER           PIC X     VALUE SPACE.                         
012500       05 T-IDARTNR        PIC Z(8)9.                                     
012600       05 FILLER           PIC X(5)  VALUE SPACE.                         
012700       05 T-IDDISTR        PIC Z(4)9.                                     
012800       05 FILLER           PIC X(4)  VALUE SPACE.                         
012900       05 T-PRARTNTO       PIC Z(6)9.99.                                  
013000       05 FILLER           PIC X(7)  VALUE SPACE.                         
013100       05 T-TIFAKT         PIC Z(6)9.                                     
013200*      05 FILLER           PIC X     VALUE SPACE.                         
013300     SKIP2                                                                
013400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500     EJECT                                                                
013600 PROCEDURE DIVISION.                                                      
013700 MAIN SECTION.                                                            
013800     SKIP2                                                                
013900                                                                          
014000     PERFORM A-INIT                                                       
014100*SKAPA REPORT                                                             
014200     PERFORM B-SKAPA-REPORT                                               
014300                                                                          
014400     PERFORM Z-FINIT                                                      
014500                                                                          
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     OPEN INPUT  W47654                                                   
015300                                                                          
015400     OPEN OUTPUT W47657                                                   
015500     SKIP2                                                                
015600     ACCEPT DAGENS-DATUM  FROM DATE                                       
015700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015800     .                                                                    
015900     EJECT                                                                
016000 B-SKAPA-REPORT SECTION.                                                  
016100     PERFORM S01-LAES-W47654                                              
016200     PERFORM UNTIL IN01-KDPRTYP = 'T' OR END-OF-W47654                    
016300       PERFORM S01-LAES-W47654                                            
016400     END-PERFORM                                                          
016500     IF IN01-KDPRTYP = 'T'                                                
016600       MOVE TOMRAD TO           REP-AREA                                  
016700       PERFORM S11-SKRIV-W47657                                           
016800       MOVE T-RUB03          TO  REP-AREA                                 
016900       PERFORM S11-SKRIV-W47657                                           
017000       MOVE TOMRAD TO           REP-AREA                                  
017100       PERFORM S11-SKRIV-W47657                                           
017200       MOVE T-RUB01          TO  REP-AREA                                 
017300       PERFORM S11-SKRIV-W47657                                           
017400       MOVE TOMRAD           TO  REP-AREA                                 
017500       PERFORM S11-SKRIV-W47657                                           
017600     END-IF                                                               
017700     PERFORM UNTIL END-OF-W47654                                          
017800       IF IN01-KDPRTYP = 'T'                                              
017900         MOVE IN01-IDARTNR     TO T-IDARTNR                               
018000         MOVE IN01-IDDISTR     TO T-IDDISTR                               
018100         MOVE IN01-PRARTNTO    TO T-PRARTNTO                              
018200         MOVE IN01-TIFAKT      TO T-TIFAKT                                
018300         MOVE T-RAD01          TO  REP-AREA                               
018400         PERFORM S11-SKRIV-W47657                                         
018500       END-IF                                                             
018600       PERFORM S01-LAES-W47654                                            
018700     END-PERFORM                                                          
018800     .                                                                    
018900     EJECT                                                                
019000 Z-FINIT SECTION.                                                         
019100     CLOSE W47654                                                         
019200           W47657                                                         
019300     SKIP2                                                                
019400     MOVE 'S' TO POSTSUM-OPKOD                                            
019500     CALL POSTSUM USING POSTSUM-PARM                                      
019600     .                                                                    
019700     EJECT                                                                
019800 S01-LAES-W47654  SECTION.                                                
019900     READ W47654 INTO IN01-AREA                                           
020000     AT END                                                               
020100        MOVE HIGH-VALUE TO IN01-AREA                                      
020200        SET END-OF-W47654 TO TRUE                                         
020300                                                                          
020400     NOT AT END                                                           
020500        MOVE 'W47654' TO POSTSUM-FDNAMN                                   
020600        MOVE 'W47657D1' TO POSTSUM-DDNAMN2                                
020700*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
020800*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
020900        MOVE IN01-IDPTYP TO POSTSUM-TRANSTYP                              
021000        CALL POSTSUM USING POSTSUM-PARM                                   
021100     END-READ                                                             
021200     .                                                                    
021300     EJECT                                                                
021400 S11-SKRIV-W47657 SECTION.                                                
021500                                                                          
021600     WRITE REP-POST FROM REP-AREA                                         
021700*    DISPLAY REP-AREA                                                     
021800     MOVE 'REP'      TO POSTSUM-TRANSTYP                                  
021900     MOVE 'W47657'   TO POSTSUM-FDNAMN                                    
022000     MOVE 'W47657D2' TO POSTSUM-DDNAMN2                                   
022100     CALL POSTSUM USING POSTSUM-PARM                                      
022200     .                                                                    
022300     EJECT                                                                
022400*                                                                         
022500*S99-ABEND SECTION.                                                       
022600*                                                                         
022700*    SKIP2                                                                
022800*    MOVE 'S' TO POSTSUM-OPKOD                                            
022900*    CALL POSTSUM USING POSTSUM-PARM                                      
023000*    CALL ABEND USING RKOD-ABEND                                          
023100*    .                                                                    
