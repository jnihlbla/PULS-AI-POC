000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5550200.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   98/01/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER BASEN WDL9/WLLOGA (SALDOUPPDAT)                 
000900*        OCH SKAPAR EN UTFIL W55502, RENSNINGSFIL                         
001000*        SAMT EN UTFIL W55501, VECKANS SPARADE SALDOFIL.                  
001100*VILKOR1:ALLA POSTER SOM ÄR ÄLDRE ÄN 15 ARBETSDAGAR.                      
001300*VILKOR2:VILKOR2 NO LONGER ACTIVE (25/01/25)                              
001300*        ALLA POSTER SOM ÄR ÄLDRE ÄN 5 ARBETSDAGAR OCH                    
001400*        LOGG-DAREGDAT-LADD > 0 SKALL TILL RENS-FILEN                     
001500*        PROGRAMMET LÄSER      WLLOGA (WDL9)                              
001600*VILKOR3:TESTA OM ÅRSKÖRNING FÖR SOL SKALL KÖRAS                          
001700*        KÖR WORKDAY MED 5 ARBETSDAGAR FÄRRE ÄN VILLKOR1                  
001800*        OM ÅR OLIKA PÅ DESSA TVÅ DATUM, STARTAS ÅRSKÖRNINGEN.            
001900*                                                                         
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- RENSNINGSFIL                                               
003300     SELECT W55501                     ASSIGN TO W55502D1.                
003400*          --- VECKANS SPARADE TRANSAR                                    
003500     SELECT W55502                     ASSIGN TO W55502D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W55501                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W55502 -PRE  SP-   -L.                                    
004600     SKIP3                                                                
004700 FD  W55502                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  POST -COPY W55502 -PRE  UT-    -L.                                   
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500*    -COPY WY2000W1                                                       
005600     SKIP3                                                                
005700 77  IDPGM                       PIC X(8)    VALUE 'W5550200'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  RENSA-FLAGGA                PIC X       VALUE 'N'.                   
006100                                                                          
006200 01  TRANS-DATUM                 PIC 9(8)    VALUE ZERO.                  
006300 01  FILLER REDEFINES TRANS-DATUM.                                        
006400     03  TRANS-DATUM-AA1         PIC 9(2).                                
006500     03  TRANS-DATUM-AAMMDD      PIC 9(6).                                
006600                                                                          
007200 01  W-DAGENS-DATUM-AAMMDD       PIC 9(6)    VALUE ZERO.                  
007300 01  W-TRANSDATUM-15             PIC 9(6)    VALUE ZERO.                  
007400 01  W-TRANSDATUM-10             PIC 9(6)    VALUE ZERO.                  
007600     EJECT                                                                
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800*                                                                         
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
008400     03  W980SOP                 PIC X(8)    VALUE 'W980SOP '.            
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL ABEND                                            
008700                                                                          
008800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009100     SKIP2                                                                
009200 01  FELTEXT.                                                             
009300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009500     EJECT                                                                
009600*    --- VALID IDDC CODES                                                 
009700*                                                                         
009800*01  -COPY WWDCKONS                                                       
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL POSTSUM                                          
010100*                                                                         
010200*01  -COPY W0005   -PRE  POSTSUM-                                         
010300     EJECT                                                                
010400 01  UT-AREA-START               PIC X(24)   VALUE                        
010500                                 'UT-AREA-START  '.                       
010600*01  AREA -COPY W55502     -PRE UT-                                       
010700     EJECT                                                                
010800 01  FILLER                      PIC X(24)   VALUE 'SPARFILAREA'.         
010900                                                                          
011000*01  AREA -COPY W55502     -PRE SP-                                       
011100     EJECT                                                                
011200                                                                          
011300 01  FILLER                     PIC X(24)  VALUE 'WORKAREA-START'.        
011400*01  -COPY WORKAREA                                                       
011500     EJECT                                                                
011600                                                                          
011700 01  FILLER                     PIC X(24)  VALUE 'WSOPAREA-START'.        
011800*01  -COPY WSOPAREA                                                       
011900     EJECT                                                                
012000                                                                          
012100     SKIP2                                                                
012200                                                                          
012300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012600     SKIP3                                                                
012700*    --- STATUS-KOD FRÅN IMS                                              
012800 01  STATUS-WS                   PIC XX.                                  
012900     88  SEGMENT-FINNS                       VALUE '  '.                  
013000     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
013100     SKIP2                                                                
013200 01  GODK-STATUSKODER.                                                    
013300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400     SKIP3                                                                
013500 01  SSA1                        PIC X(64).                               
013600 01  SSA2                        PIC X(64).                               
013700     EJECT                                                                
013800*    --- IMS FUNKTIONSKODER                                               
013900*01  -COPY W0003                                                          
014000     EJECT                                                                
014100*    ---  DLI INPUT-OUTPUT AREA                                           
014200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA'.                      
014300 01  DLI-IO-WLLOGA.                                                       
014400                                                                          
014500*      05  -COPY WDL901                                                   
014600     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800                                                                          
014900     EJECT                                                                
015000*01  -COPY W0008  -PRE LOGA-                                              
015100     05  FILLER                  PIC X.                                   
015200     EJECT                                                                
015300 PROCEDURE DIVISION  USING LOGA-PCB.                                      
015400 MAIN SECTION.                                                            
015500     ENTRY 'DLITCBL' USING LOGA-PCB.                                      
015600                                                                          
015700                                                                          
015800     PERFORM A-INIT                                                       
015900                                                                          
016000     PERFORM IMS-GET-LOGA                                                 
016100     PERFORM UNTIL SEGMENT-SAKNAS                                         
016200       EVALUATE LOGA-SEG-NAME-FB                                          
016300         WHEN 'WDL901'                                                    
016400           PERFORM B-KOLLA-DATUM                                          
016500       END-EVALUATE                                                       
016600       PERFORM IMS-GET-LOGA                                               
016700     END-PERFORM                                                          
016800     PERFORM Z-FINIT                                                      
016900                                                                          
017000     MOVE ZERO TO RETURN-CODE                                             
017100     GOBACK                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 A-INIT SECTION.                                                          
017500                                                                          
017600     OPEN OUTPUT W55501                                                   
017700                 W55502                                                   
017800                                                                          
017900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018000                                                                          
018100     MOVE FUNCTION CURRENT-DATE(3:6) TO W-DAGENS-DATUM-AAMMDD             
018200     DISPLAY 'DAGENS-DATUM-6 ' W-DAGENS-DATUM-AAMMDD                      
018300                                                                          
018400     MOVE WC-CDC-SE                  TO WORK-IDDC                         
018500     MOVE 15                         TO WORK-KVWORKD                      
018600     MOVE W-DAGENS-DATUM-AAMMDD      TO WORK-TIAAMMDD-TOM                 
018700     MOVE 003                        TO WORK-KDCALL                       
018800     CALL WORKDAY  USING                WORK-KDCALL                       
018900                                        WORK-DATE-AREA                    
019000                                        WORK-KDSVAR                       
019100     END-CALL                                                             
019200     IF WORK-KDSVAR-FEL                                                   
019300        MOVE ' FEL FRÅN WORKDAY  I A-INIT 1 ' TO FELTEXT                  
019400        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
019500     END-IF                                                               
019600     MOVE WORK-TIAAMMDD-FOM          TO W-TRANSDATUM-15                   
019800     DISPLAY 'TRANSDATUM-15 ' W-TRANSDATUM-15                             
019900                                                                          
020000****  TESTA OM ÅRSKÖRNING SKALL KÖRAS                                     
020100     MOVE 10                         TO WORK-KVWORKD                      
020200     MOVE W-DAGENS-DATUM-AAMMDD      TO WORK-TIAAMMDD-TOM                 
020300     MOVE 003                        TO WORK-KDCALL                       
020400     CALL WORKDAY  USING                WORK-KDCALL                       
020500                                        WORK-DATE-AREA                    
020600                                        WORK-KDSVAR                       
020700     END-CALL                                                             
020800     IF WORK-KDSVAR-FEL                                                   
020900        MOVE ' FEL FRÅN WORKDAY  I A-INIT 1 ' TO FELTEXT                  
021000        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
021100     END-IF                                                               
021200     MOVE WORK-TIAAMMDD-FOM          TO W-TRANSDATUM-10                   
021300     DISPLAY 'TRANSDATUM-10 ' W-TRANSDATUM-10                             
021400                                                                          
021410     IF W-TRANSDATUM-10(1:2) NOT = W-TRANSDATUM-15(1:2)                   
021500***  OM ÅRSSKIFTE RENSA ENDAST FÖR GAMMALT ÅR                             
021600***  STARTA SEDAN W555Y1                                                  
021800       MOVE 1231                     TO W-TRANSDATUM-15(3:4)              
021900       DISPLAY 'KORRIGERAT TRANSDATUM-15 ' W-TRANSDATUM-15                
022000                                                                          
022100       MOVE SPACE                TO  SOP-DDPREFIX                         
022200       MOVE 'O'                  TO  SOP-SOPFUNC                          
022300       MOVE ZERO                 TO  SOP-ACTPASS-DATE                     
022400       MOVE 'W555Y1'             TO  SOP-PROC-NAME                        
022500       CALL W980SOP USING SOP-PARM-AREA                                   
022600                                                                          
022700     END-IF                                                               
022800     .                                                                    
022900*** END TESTA ÅRSKÖRNING                                                  
023000                                                                          
024700                                                                          
024800 B-KOLLA-DATUM SECTION.                                                   
024900                                                                          
025000     MOVE NEJ TO RENSA-FLAGGA                                             
5200       COMPUTE TRANS-DATUM = 99999999 - LOGG-DAREGDAT-9KOMPL                
5300       MOVE W-TRANSDATUM-15         TO TMP1-YYMMDD                          
5400       MOVE TRANS-DATUM-AAMMDD      TO TMP2-YYMMDD                          
5500       PERFORM WY2000P1                                                     
5600       IF TMP1-YYMMDD > TMP2-YYMMDD                                         
5700         MOVE 'JA'                  TO RENSA-FLAGGA                         
5800       END-IF                                                               
027200                                                                          
027300     IF RENSA-FLAGGA = 'J'                                                
027400       MOVE DLI-IO-WLLOGA TO UT-AREA                                      
027500       PERFORM S02-SKRIV-W55502                                           
027600                                                                          
7800         MOVE DLI-IO-WLLOGA TO SP-AREA                                      
7900         PERFORM S01-SKRIV-W55501                                           
028100     END-IF                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 Z-FINIT SECTION.                                                         
028500                                                                          
028600     CLOSE W55501                                                         
028700           W55502                                                         
028800                                                                          
028900     MOVE 'S' TO POSTSUM-OPKOD                                            
029000     CALL POSTSUM USING POSTSUM-PARM                                      
029100     .                                                                    
029200     EJECT                                                                
029300 S01-SKRIV-W55501 SECTION.                                                
029400                                                                          
029500     WRITE SP-POST FROM SP-AREA                                           
029600                                                                          
029700     MOVE 'SPAR'     TO POSTSUM-TRANSTYP                                  
029800     MOVE 'W55501'   TO POSTSUM-FDNAMN                                    
029900     MOVE 'W55502D1' TO POSTSUM-DDNAMN2                                   
030000     CALL POSTSUM USING POSTSUM-PARM                                      
030100     .                                                                    
030200     EJECT                                                                
030300 S02-SKRIV-W55502 SECTION.                                                
030400                                                                          
030500     WRITE UT-POST FROM UT-AREA                                           
030600                                                                          
030700     MOVE 'RENS'     TO POSTSUM-TRANSTYP                                  
030800     MOVE 'W55502'   TO POSTSUM-FDNAMN                                    
030900     MOVE 'W55502D2' TO POSTSUM-DDNAMN2                                   
031000     CALL POSTSUM USING POSTSUM-PARM                                      
031100     .                                                                    
031200     EJECT                                                                
031300* --- IMS SEKTIONER ---                                                   
031400     SKIP3                                                                
031500 IMS-GET-LOGA   SECTION.                                                  
031600                                                                          
031700     CALL CBLTDLI USING GN LOGA-PCB DLI-IO-WLLOGA                         
031800     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
031900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
032000     PERFORM IMS-STATUSKONTROLL                                           
032100     .                                                                    
032200     EJECT                                                                
032300 IMS-STATUSKONTROLL SECTION.                                              
032400                                                                          
032500     SET STATUS-IX TO 1                                                   
032600     SEARCH GODK-STATUS                                                   
032700       AT END                                                             
032800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032900           DELIMITED BY SIZE INTO FELTEXT                                 
033000         DISPLAY FELTEXT                                                  
033100         CALL FELLOG                                                      
033200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033300         CONTINUE                                                         
033400     END-SEARCH                                                           
033500     .                                                                    
033600*    -COPY WY2000P1                                                       
