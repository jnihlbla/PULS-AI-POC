000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4639600.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/07/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        REDIGERA LISTA FÖR DIREKTLEVERANSLARM                            
000810*        FÖR VARJE LEVERANTÖR SKICKAS DATA TILL D&P, VIA DAP3,            
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDF4 (DIREKTLEVERANSER)                    
001100*                                                                         
001200* INFIL W4639J - UPPFÖLJNINGSFIL SENA OCH FREKVENT                        
001300*                OMPLANERADE DIREKTLEVERANSRADER                          
001400* URFIL w4639L - LARMPOSTER TILL D&P                                      
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900                                                                          
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*          --- DIREKTLEVERANSLARM                                         
002800     SELECT W4639J                     ASSIGN TO W46396D1.                
002900*          --- DIREKTLEVERANSLARM D&P                                     
003000     SELECT w4639L                     ASSIGN TO W46396D2.                
003100                                                                          
003200 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400                                                                          
003500 FD  W4639J                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800*01  POST      -COPY W4639J -PRE  LARM-  -L.                              
003900                                                                          
004000 FD  w4639L                                                               
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS  0.                                                   
004300 01  DOP-POST  PIC X(100).                                                
004400                                                                          
004500                                                                          
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W4639600'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005200 77  CURRENT-IDLEVNR             PIC X(5)    VALUE SPACE.                 
005300 77  CURRENT-IDPTYP              PIC X(3)    VALUE SPACE.                 
005400                                                                          
005500 77  W4639J-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  EOF-W4639J                          VALUE 'J'.                   
005700                                                                          
005800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES DAGENS-DATUM.                                       
006000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006300                                                                          
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500                                                                          
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000                                                                          
007100*    --- PARAMETRAR TILL ABEND                                            
007200                                                                          
007300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007600                                                                          
007700 01  FELTEXT.                                                             
007800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008000                                                                          
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400                                                                          
008500 01  LARM-AREA-START             PIC X(24)   VALUE                        
008600                                 'LARM-AREA-START  '.                     
008700                                                                          
008800*01  AREA -COPY W4639J     -PRE LARM-                                     
008900                                                                          
009000 01  BLANK-RAD.                                                           
009100     03  FILLER            PIC X(01) VALUE SPACE.                         
009200     03  FILLER  PIC X     VALUE X'5E'.                                   
009300                                                                          
009400 01  RUBRIK-IDLEVNR.                                                      
009500     03  FILLER            PIC X(35) VALUE                                
009600        ' Late Direct Deliveries  Supplier: '.                            
009700     03  RUB-IDLEVNR       PIC X(5) VALUE SPACE.                          
009800     03  FILLER  PIC X     VALUE X'5E'.                                   
009900                                                                          
010000 01  RUBRIK-TYP-001.                                                      
010100     03  FILLER            PIC X(27) VALUE                                
010200        ' Not performed annullations'.                                    
010300     03  FILLER  PIC X     VALUE X'05'.                                   
010400 01  RUBRIK2-TYP-001.                                                     
010500     03  FILLER            PIC X(06) VALUE  ' Distr'.                     
010600     03  FILLER            PIC X     VALUE X'5E'.                         
010700     03  FILLER            PIC X(07) VALUE  'Part no'.                    
010800     03  FILLER            PIC X     VALUE X'5E'.                         
010900     03  FILLER            PIC X(04) VALUE  'Cust'.                       
011000     03  FILLER            PIC X     VALUE X'5E'.                         
011100     03  FILLER            PIC X(05) VALUE  'Order'.                      
011200     03  FILLER            PIC X     VALUE X'5E'.                         
011300     03  FILLER            PIC X(07) VALUE  'Prod no'.                    
011400     03  FILLER            PIC X     VALUE X'5E'.                         
011500     03  FILLER            PIC X(07) VALUE  'Line no'.                    
011600     03  FILLER            PIC X     VALUE X'5E'.                         
011700     03  FILLER            PIC X(07) VALUE  'Reg dat'.                    
011800     03  FILLER            PIC X     VALUE X'5E'.                         
011900     03  FILLER            PIC X(03) VALUE  'RFS'.                        
012000     03  FILLER            PIC X     VALUE X'5E'.                         
012100     03  FILLER            PIC X(08) VALUE  'Del date'.                   
012200     03  FILLER            PIC X     VALUE X'5E'.                         
012300     03  FILLER            PIC X(03) VALUE  'Chg'.                        
012400     03  FILLER            PIC X     VALUE X'5E'.                         
012500     03  FILLER            PIC X(06) VALUE  'Annull'.                     
012600     03  FILLER            PIC X     VALUE X'5E'.                         
012700     03  FILLER            PIC X(04) VALUE  '>2YR'.                       
012800     03  FILLER            PIC X     VALUE X'5E'.                         
012900                                                                          
013000                                                                          
013100 01  RUBRIK-TYP-002.                                                      
013200     03  FILLER            PIC X(26) VALUE                                
013300        ' Late replanned deliveries'.                                     
013400     03  FILLER  PIC X     VALUE X'5E'.                                   
013500 01  RUBRIK2-TYP-002.                                                     
013600     03  FILLER            PIC X(06) VALUE  ' Distr'.                     
013700     03  FILLER            PIC X     VALUE X'5E'.                         
013800     03  FILLER            PIC X(07) VALUE  'Part no'.                    
013900     03  FILLER            PIC X     VALUE X'5E'.                         
014000     03  FILLER            PIC X(04) VALUE  'Cust'.                       
014100     03  FILLER            PIC X     VALUE X'5E'.                         
014200     03  FILLER            PIC X(05) VALUE  'Order'.                      
014300     03  FILLER            PIC X     VALUE X'5E'.                         
014400     03  FILLER            PIC X(07) VALUE  'Prod no'.                    
014500     03  FILLER            PIC X     VALUE X'5E'.                         
014600     03  FILLER            PIC X(07) VALUE  'Line no'.                    
014700     03  FILLER            PIC X     VALUE X'5E'.                         
014800     03  FILLER            PIC X(07) VALUE  'Reg dat'.                    
014900     03  FILLER            PIC X     VALUE X'5E'.                         
015000     03  FILLER            PIC X(03) VALUE  'RFS'.                        
015100     03  FILLER            PIC X     VALUE X'5E'.                         
015200     03  FILLER            PIC X(08) VALUE  'Del date'.                   
015300     03  FILLER            PIC X     VALUE X'5E'.                         
015400     03  FILLER            PIC X(03) VALUE  'Chg'.                        
015500     03  FILLER            PIC X     VALUE X'5E'.                         
015600     03  FILLER            PIC X(04) VALUE  '>2YR'.                       
015700     03  FILLER            PIC X     VALUE X'5E'.                         
015800                                                                          
015900 01  RUBRIK-TYP-003.                                                      
016000     03  FILLER            PIC X(16) VALUE                                
016100        ' Late deliveries'.                                               
016200     03  FILLER  PIC X     VALUE X'5E'.                                   
016300 01  RUBRIK2-TYP-003.                                                     
016400     03  FILLER            PIC X(06) VALUE  ' Distr'.                     
016500     03  FILLER            PIC X     VALUE X'5E'.                         
016600     03  FILLER            PIC X(07) VALUE  'Part no'.                    
016700     03  FILLER            PIC X     VALUE X'5E'.                         
016800     03  FILLER            PIC X(04) VALUE  'Cust'.                       
016900     03  FILLER            PIC X     VALUE X'5E'.                         
017000     03  FILLER            PIC X(05) VALUE  'Order'.                      
017100     03  FILLER            PIC X     VALUE X'5E'.                         
017200     03  FILLER            PIC X(07) VALUE  'Prod no'.                    
017300     03  FILLER            PIC X     VALUE X'5E'.                         
017400     03  FILLER            PIC X(07) VALUE  'Line no'.                    
017500     03  FILLER            PIC X     VALUE X'5E'.                         
017600     03  FILLER            PIC X(07) VALUE  'Reg dat'.                    
017700     03  FILLER            PIC X     VALUE X'5E'.                         
017800     03  FILLER            PIC X(03) VALUE  'RFS'.                        
017900     03  FILLER            PIC X     VALUE X'5E'.                         
017910     03  FILLER            PIC X(10) VALUE  'Req Annull'.                 
017920     03  FILLER            PIC X     VALUE X'5E'.                         
018000     03  FILLER            PIC X(04) VALUE  '>2YR'.                       
018100     03  FILLER            PIC X     VALUE X'5E'.                         
018200                                                                          
018300 01  RUBRIK-TYP-004.                                                      
018400     03  FILLER            PIC X(30) VALUE                                
018500        ' Frequent replanned deliveries'.                                 
018600     03  FILLER  PIC X     VALUE X'5E'.                                   
018700 01  RUBRIK2-TYP-004.                                                     
018800     03  FILLER            PIC X(06) VALUE  ' Distr'.                     
018900     03  FILLER            PIC X     VALUE X'5E'.                         
019000     03  FILLER            PIC X(07) VALUE  'Part no'.                    
019100     03  FILLER            PIC X     VALUE X'5E'.                         
019200     03  FILLER            PIC X(04) VALUE  'Cust'.                       
019300     03  FILLER            PIC X     VALUE X'5E'.                         
019400     03  FILLER            PIC X(05) VALUE  'Order'.                      
019500     03  FILLER            PIC X     VALUE X'5E'.                         
019600     03  FILLER            PIC X(07) VALUE  'Prod no'.                    
019700     03  FILLER            PIC X     VALUE X'5E'.                         
019800     03  FILLER            PIC X(07) VALUE  'Line no'.                    
019900     03  FILLER            PIC X     VALUE X'5E'.                         
020000     03  FILLER            PIC X(07) VALUE  'Reg dat'.                    
020100     03  FILLER            PIC X     VALUE X'5E'.                         
020200     03  FILLER            PIC X(03) VALUE  'RFS'.                        
020300     03  FILLER            PIC X     VALUE X'5E'.                         
020400     03  FILLER            PIC X(08) VALUE  'Del date'.                   
020500     03  FILLER            PIC X     VALUE X'5E'.                         
020600     03  FILLER            PIC X(03) VALUE  'Chg'.                        
020700     03  FILLER            PIC X     VALUE X'5E'.                         
020800     03  FILLER            PIC X(04) VALUE  '>2YR'.                       
020900     03  FILLER            PIC X     VALUE X'5E'.                         
021000                                                                          
021100 01  RAD-001.                                                             
021200     03  FILLER            PIC X    VALUE SPACE.                          
021300     03  001-IDDISTR       PIC 9(4).                                      
021400     03  FILLER            PIC X    VALUE X'5E'.                          
021500     03  001-IDARTNR       PIC Z(8)9.                                     
021600     03  FILLER            PIC X    VALUE X'5E'.                          
021700     03  001-IDKUNDNR      PIC Z(5)9.                                     
021800     03  FILLER            PIC X    VALUE X'5E'.                          
021900     03  001-IDORDNR5      PIC Z(4)9.                                     
022000     03  FILLER            PIC X    VALUE X'5E'.                          
022100     03  001-IDPRODNR      PIC Z(6)9.                                     
022200     03  FILLER            PIC X    VALUE X'5E'.                          
022300     03  001-IDPURAD       PIC Z(3)9.                                     
022400     03  FILLER            PIC X    VALUE X'5E'.                          
022500     03  001-TIUTSKR       PIC 9(6).                                      
022600     03  FILLER            PIC X    VALUE X'5E'.                          
022700     03  001-TISKEPPN      PIC 9(6).                                      
022800     03  FILLER            PIC X    VALUE X'5E'.                          
022900     03  001-TISLULEV      PIC Z(6).                                      
023000     03  FILLER            PIC X    VALUE X'5E'.                          
023100     03  001-KVSLULEV      PIC Z(3).                                      
023200     03  FILLER            PIC X    VALUE X'5E'.                          
023300     03  001-TIANNULL      PIC 9(6).                                      
023400     03  FILLER            PIC X    VALUE X'5E'.                          
023500     03  001-KDOLD         PIC X(1).                                      
023600     03  FILLER            PIC X    VALUE X'5E'.                          
023700                                                                          
023800 01  RAD-002.                                                             
023900     03  FILLER            PIC X    VALUE SPACE.                          
024000     03  002-IDDISTR       PIC 9(4).                                      
024100     03  FILLER            PIC X    VALUE X'5E'.                          
024200     03  002-IDARTNR       PIC Z(8)9.                                     
024300     03  FILLER            PIC X    VALUE X'5E'.                          
024400     03  002-IDKUNDNR      PIC Z(5)9.                                     
024500     03  FILLER            PIC X    VALUE X'5E'.                          
024600     03  002-IDORDNR5      PIC Z(4)9.                                     
024700     03  FILLER            PIC X    VALUE X'5E'.                          
024800     03  002-IDPRODNR      PIC Z(6)9.                                     
024900     03  FILLER            PIC X    VALUE X'5E'.                          
025000     03  002-IDPURAD       PIC Z(3)9.                                     
025100     03  FILLER            PIC X    VALUE X'5E'.                          
025200     03  002-TIUTSKR       PIC 9(6).                                      
025300     03  FILLER            PIC X    VALUE X'5E'.                          
025400     03  002-TISKEPPN      PIC 9(6).                                      
025500     03  FILLER            PIC X    VALUE X'5E'.                          
025600     03  002-TISLULEV      PIC Z(6).                                      
025700     03  FILLER            PIC X    VALUE X'5E'.                          
025800     03  002-KVSLULEV      PIC Z(3).                                      
025900     03  FILLER            PIC X    VALUE X'5E'.                          
026000     03  002-KDOLD         PIC X(1).                                      
026100     03  FILLER            PIC X    VALUE X'5E'.                          
026200                                                                          
026300 01  RAD-003.                                                             
026400     03  FILLER            PIC X    VALUE SPACE.                          
026500     03  003-IDDISTR       PIC 9(4).                                      
026600     03  FILLER            PIC X    VALUE X'5E'.                          
026700     03  003-IDARTNR       PIC Z(8)9.                                     
026800     03  FILLER            PIC X    VALUE X'5E'.                          
026900     03  003-IDKUNDNR      PIC Z(5)9.                                     
027000     03  FILLER            PIC X    VALUE X'5E'.                          
027100     03  003-IDORDNR5      PIC Z(4)9.                                     
027200     03  FILLER            PIC X    VALUE X'5E'.                          
027300     03  003-IDPRODNR      PIC Z(6)9.                                     
027400     03  FILLER            PIC X    VALUE X'5E'.                          
027500     03  003-IDPURAD       PIC Z(3)9.                                     
027600     03  FILLER            PIC X    VALUE X'5E'.                          
027700     03  003-TIUTSKR       PIC 9(6).                                      
027800     03  FILLER            PIC X    VALUE X'5E'.                          
027900     03  003-TISKEPPN      PIC 9(6).                                      
028000     03  FILLER            PIC X    VALUE X'5E'.                          
028010     03  003-ANNULL.                                                      
028011         05 003-TIANNULL   PIC Z(6).                                      
028012         05 FILLER         PIC X    VALUE SPACE.                          
028013         05 003-KDANNULL   PIC X.                                         
028020     03  FILLER            PIC X    VALUE X'5E'.                          
028100     03  003-KDOLD         PIC X(1).                                      
028200     03  FILLER            PIC X    VALUE X'5E'.                          
028300                                                                          
028400 01  RAD-004.                                                             
028500     03  FILLER            PIC X    VALUE SPACE.                          
028600     03  004-IDDISTR       PIC 9(4).                                      
028700     03  FILLER            PIC X    VALUE X'5E'.                          
028800     03  004-IDARTNR       PIC Z(8)9.                                     
028900     03  FILLER            PIC X    VALUE X'5E'.                          
029000     03  004-IDKUNDNR      PIC Z(5)9.                                     
029100     03  FILLER            PIC X    VALUE X'5E'.                          
029200     03  004-IDORDNR5      PIC Z(4)9.                                     
029300     03  FILLER            PIC X    VALUE X'5E'.                          
029400     03  004-IDPRODNR      PIC Z(6)9.                                     
029500     03  FILLER            PIC X    VALUE X'5E'.                          
029600     03  004-IDPURAD       PIC Z(3)9.                                     
029700     03  FILLER            PIC X    VALUE X'5E'.                          
029800     03  004-TIUTSKR       PIC 9(6).                                      
029900     03  FILLER            PIC X    VALUE X'5E'.                          
030000     03  004-TISKEPPN      PIC 9(6).                                      
030100     03  FILLER            PIC X    VALUE X'5E'.                          
030200     03  004-TISLULEV      PIC Z(6).                                      
030300     03  FILLER            PIC X    VALUE X'5E'.                          
030400     03  004-KVSLULEV      PIC Z(3).                                      
030500     03  FIlLER            PIC X    VALUE X'5E'.                          
030600     03  004-KDOLD         PIC X(1).                                      
030700     03  FILLER            PIC X    VALUE X'5E'.                          
030710                                                                          
030720 01  W001-DAP.                                                            
030730     03  FILLER                  PIC X(165)  VALUE SPACE.                 
030800                                                                          
030900                                                                          
031000                                                                          
031100 PROCEDURE DIVISION.                                                      
031200 MAIN SECTION.                                                            
031300                                                                          
031400                                                                          
031500     PERFORM A-INIT                                                       
031600                                                                          
031700     PERFORM S01-READ-W4639J                                              
031800     PERFORM UNTIL EOF-W4639J                                             
031900                                                                          
032000        IF LARM-IDLEVNR NOT = CURRENT-IDLEVNR                             
032100           PERFORM B-NY-LEVERANTOR                                        
032110           PERFORM C-NY-POSTTYP                                           
032200        END-IF                                                            
032300        IF LARM-IDPTYP NOT = CURRENT-IDPTYP                               
032400           PERFORM C-NY-POSTTYP                                           
032500        END-IF                                                            
032600        PERFORM D-NY-LARMPOST                                             
032700                                                                          
032800        PERFORM S01-READ-W4639J                                           
032900     END-PERFORM                                                          
033000                                                                          
033100     PERFORM Z-FINIT                                                      
033200                                                                          
033300     MOVE ZERO TO RETURN-CODE                                             
033400     GOBACK                                                               
033500     .                                                                    
033600                                                                          
033700                                                                          
033800 A-INIT SECTION.                                                          
033900     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
034000                                                                          
034100     OPEN INPUT  W4639J                                                   
034200     OPEN OUTPUT w4639L                                                   
034300                                                                          
034400     ACCEPT DAGENS-DATUM  FROM DATE                                       
034500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
034600     .                                                                    
034700                                                                          
034800                                                                          
034900 B-NY-LEVERANTOR    SECTION.                                              
035000     MOVE 'B-NY-LEVERANTOR ' TO CURRENT-SECTION                           
035100                                                                          
035101     MOVE '¤DAPDL-LARM'      TO W001-DAP                                  
035102     WRITE DOP-POST FROM W001-DAP                                         
035103                                                                          
035104     MOVE SPACE              TO W001-DAP                                  
035105     STRING '¤DAP' LARM-IDLEVNR                                           
035106     DELIMITED BY SIZE INTO W001-DAP                                      
035107     WRITE DOP-POST FROM W001-DAP                                         
035108                                                                          
035110     IF RUB-IDLEVNR NOT = SPACE                                           
035120        WRITE DOP-POST FROM BLANK-RAD                                     
035121        WRITE DOP-POST FROM BLANK-RAD                                     
035130     END-IF                                                               
035200     MOVE LARM-IDLEVNR       TO RUB-IDLEVNR                               
035300     WRITE DOP-POST FROM RUBRIK-IDLEVNR                                   
035400     MOVE 'w4639L'       TO POSTSUM-FDNAMN                                
035500     MOVE 'W46396D2'     TO POSTSUM-DDNAMN2                               
035600     MOVE 'LEV'          TO POSTSUM-TRANSTYP                              
035700     CALL POSTSUM USING     POSTSUM-PARM                                  
035900     MOVE LARM-IDLEVNR   TO CURRENT-IDLEVNR                               
036000     .                                                                    
036100                                                                          
036200                                                                          
036300 C-NY-POSTTYP       SECTION.                                              
036400     MOVE 'C-NY-POSTTYP    ' TO CURRENT-SECTION                           
036500                                                                          
036600     IF LARM-IDPTYP = '001'                                               
036700        WRITE DOP-POST FROM RUBRIK-TYP-001                                
036800        WRITE DOP-POST FROM BLANK-RAD                                     
036900        WRITE DOP-POST FROM RUBRIK2-TYP-001                               
037000     END-IF                                                               
037100     IF LARM-IDPTYP = '002'                                               
037200        WRITE DOP-POST FROM BLANK-RAD                                     
037300        WRITE DOP-POST FROM RUBRIK-TYP-002                                
037400        WRITE DOP-POST FROM BLANK-RAD                                     
037500        WRITE DOP-POST FROM RUBRIK2-TYP-002                               
037600     END-IF                                                               
037700     IF LARM-IDPTYP = '003'                                               
037710        WRITE DOP-POST FROM BLANK-RAD                                     
037800        WRITE DOP-POST FROM RUBRIK-TYP-003                                
037900        WRITE DOP-POST FROM BLANK-RAD                                     
038000        WRITE DOP-POST FROM RUBRIK2-TYP-003                               
038100     END-IF                                                               
038200     IF LARM-IDPTYP = '004'                                               
038210        WRITE DOP-POST FROM BLANK-RAD                                     
038300        WRITE DOP-POST FROM RUBRIK-TYP-004                                
038400        WRITE DOP-POST FROM BLANK-RAD                                     
038500        WRITE DOP-POST FROM RUBRIK2-TYP-004                               
038600     END-IF                                                               
038700     MOVE LARM-IDPTYP        TO CURRENT-IDPTYP                            
038800     .                                                                    
038900                                                                          
039000                                                                          
039100 D-NY-LARMPOST      SECTION.                                              
039200     MOVE 'D-NY-LARMPOST   ' TO CURRENT-SECTION                           
039300                                                                          
039400     IF LARM-IDPTYP = '001'                                               
039500        PERFORM DA-REDIGERA-TYP-001                                       
039600        WRITE DOP-POST FROM RAD-001                                       
039700     END-IF                                                               
039800     IF LARM-IDPTYP = '002'                                               
039900        PERFORM DB-REDIGERA-TYP-002                                       
040000        WRITE DOP-POST FROM RAD-002                                       
040100     END-IF                                                               
040200     IF LARM-IDPTYP = '003'                                               
040300        PERFORM DC-REDIGERA-TYP-003                                       
040400        WRITE DOP-POST FROM RAD-003                                       
040500     END-IF                                                               
040600     IF LARM-IDPTYP = '004'                                               
040700        PERFORM DD-REDIGERA-TYP-004                                       
040800        WRITE DOP-POST FROM RAD-004                                       
040900     END-IF                                                               
041000                                                                          
041100     MOVE 'w4639L'       TO POSTSUM-FDNAMN                                
041200     MOVE 'W46396D2'     TO POSTSUM-DDNAMN2                               
041300     MOVE LARM-IDPTYP TO POSTSUM-TRANSTYP                                 
041400     CALL POSTSUM USING     POSTSUM-PARM                                  
041500     .                                                                    
041600                                                                          
041700                                                                          
041800 DA-REDIGERA-TYP-001 SECTION.                                             
041900     MOVE 'DA-TYP-001      ' TO CURRENT-SECTION                           
042000                                                                          
042100     MOVE LARM-IDDISTR       TO 001-IDDISTR                               
042200     MOVE LARM-IDARTNR       TO 001-IDARTNR                               
042300     MOVE LARM-IDKUNDNR      TO 001-IDKUNDNR                              
042400     MOVE LARM-IDORDNR5      TO 001-IDORDNR5                              
042500     MOVE LARM-IDPRODNR      TO 001-IDPRODNR                              
042600     MOVE LARM-IDPURAD       TO 001-IDPURAD                               
042700     MOVE LARM-TIUTSKR       TO 001-TIUTSKR                               
042800     MOVE LARM-TISKEPPN      TO 001-TISKEPPN                              
042900     MOVE LARM-TISLULEV      TO 001-TISLULEV                              
043000     MOVE LARM-KVSLULEV      TO 001-KVSLULEV                              
043100     MOVE LARM-TIANNULL      TO 001-TIANNULL                              
043200     MOVE LARM-KDOLD         TO 001-KDOLD                                 
043400     .                                                                    
043500                                                                          
043600                                                                          
043700 DB-REDIGERA-TYP-002 SECTION.                                             
043800     MOVE 'DB-TYP-002      ' TO CURRENT-SECTION                           
043900                                                                          
044000     MOVE LARM-IDDISTR       TO 002-IDDISTR                               
044100     MOVE LARM-IDARTNR       TO 002-IDARTNR                               
044200     MOVE LARM-IDKUNDNR      TO 002-IDKUNDNR                              
044300     MOVE LARM-IDORDNR5      TO 002-IDORDNR5                              
044400     MOVE LARM-IDPRODNR      TO 002-IDPRODNR                              
044500     MOVE LARM-IDPURAD       TO 002-IDPURAD                               
044600     MOVE LARM-TIUTSKR       TO 002-TIUTSKR                               
044700     MOVE LARM-TISKEPPN      TO 002-TISKEPPN                              
044800     MOVE LARM-TISLULEV      TO 002-TISLULEV                              
044900     MOVE LARM-KVSLULEV      TO 002-KVSLULEV                              
045000     MOVE LARM-KDOLD         TO 002-KDOLD                                 
045200     .                                                                    
045300                                                                          
045400                                                                          
045500 DC-REDIGERA-TYP-003 SECTION.                                             
045600     MOVE 'DC-TYP-003      ' TO CURRENT-SECTION                           
045700                                                                          
045800     MOVE LARM-IDDISTR       TO 003-IDDISTR                               
045900     MOVE LARM-IDARTNR       TO 003-IDARTNR                               
046000     MOVE LARM-IDKUNDNR      TO 003-IDKUNDNR                              
046100     MOVE LARM-IDORDNR5      TO 003-IDORDNR5                              
046200     MOVE LARM-IDPRODNR      TO 003-IDPRODNR                              
046300     MOVE LARM-IDPURAD       TO 003-IDPURAD                               
046400     MOVE LARM-TIUTSKR       TO 003-TIUTSKR                               
046500     MOVE LARM-TISKEPPN      TO 003-TISKEPPN                              
046501     IF LARM-KDANNULL = '2'                                               
046502        MOVE 'R'             TO 003-KDANNULL                              
046503     ELSE                                                                 
046504        MOVE SPACE           TO 003-KDANNULL                              
046505     END-IF                                                               
046510     MOVE LARM-TIANNULL      TO 003-TIANNULL                              
046600     MOVE LARM-KDOLD         TO 003-KDOLD                                 
046800     .                                                                    
046900                                                                          
047000                                                                          
047100 DD-REDIGERA-TYP-004 SECTION.                                             
047200     MOVE 'DD-TYP-004      ' TO CURRENT-SECTION                           
047300                                                                          
047400     MOVE LARM-IDDISTR       TO 004-IDDISTR                               
047500     MOVE LARM-IDARTNR       TO 004-IDARTNR                               
047600     MOVE LARM-IDKUNDNR      TO 004-IDKUNDNR                              
047700     MOVE LARM-IDORDNR5      TO 004-IDORDNR5                              
047800     MOVE LARM-IDPRODNR      TO 004-IDPRODNR                              
047900     MOVE LARM-IDPURAD       TO 004-IDPURAD                               
048000     MOVE LARM-TIUTSKR       TO 004-TIUTSKR                               
048100     MOVE LARM-TISKEPPN      TO 004-TISKEPPN                              
048200     MOVE LARM-TISLULEV      TO 004-TISLULEV                              
048300     MOVE LARM-KVSLULEV      TO 004-KVSLULEV                              
048400     MOVE LARM-KDOLD         TO 004-KDOLD                                 
048600     .                                                                    
048700                                                                          
048800                                                                          
048900 Z-FINIT SECTION.                                                         
049000     CLOSE W4639J w4639L                                                  
049100                                                                          
049200     MOVE 'S'        TO POSTSUM-OPKOD                                     
049300     CALL POSTSUM USING POSTSUM-PARM                                      
049400     .                                                                    
049500                                                                          
049600                                                                          
049700 S01-READ-W4639J  SECTION.                                                
049800     MOVE 'S01-READ-W4639J ' TO CURRENT-SECTION                           
049900                                                                          
050000     READ W4639J INTO LARM-AREA                                           
050100     AT END                                                               
050200        SET EOF-W4639J TO TRUE                                            
050300                                                                          
050400     NOT AT END                                                           
050500        MOVE 'W4639J'    TO POSTSUM-FDNAMN                                
050600        MOVE 'W46396D1'  TO POSTSUM-DDNAMN2                               
050700        MOVE LARM-IDPTYP TO POSTSUM-TRANSTYP                              
050800        CALL POSTSUM USING  POSTSUM-PARM                                  
050900     END-READ                                                             
051000     .                                                                    
