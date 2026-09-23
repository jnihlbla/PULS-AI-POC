000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3735400.                                                
000400 AUTHOR.         INGVAR SKJELBRED.                                        
000500 DATE-WRITTEN.   97/09/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET RÄKNAR UT SNITTETARBETSDAGAR                          
001100* DVS TIDEN DET TAR ATT GODKÄNNA EN BYTESRAPPORT                          
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700*    CHANGE LOG:                                                          
001800*                                                                         
001900*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
002000*      ----------------------------------------------------------         
002100*      15/04/09 - REDDY RAHUL     - CHINA EXCHANGE PHASE 2.               
002200*                                   E'TRACKER 10252358                    
002300*                                   CONSOLIDATE WEB REPORTS AND           
002400*                                   ADD CHINA REPORTS.                    
002410*      16/09/30 - ARUP  DATTA     - NDC SEATTLE.                          
002420*                                   E'TRACKER 10287310                    
002430*                                   CREATE WEB REPORT FOR DC44.           
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*          --- UNDERLAG TILL BERÄKNING AV SNITTUNDERLAGET                 
003500     SELECT W3734D                     ASSIGN TO W37354D1.                
003600     SKIP2                                                                
003700*          --- LISTOR TILL BYTES DC 11 91 41 43 51                        
003800     SELECT LISTA                      ASSIGN TO W37354D2.                
003900     SELECT LISTB                      ASSIGN TO W37354D3.                
004000     SELECT LISTC                      ASSIGN TO W37354D4.                
004200     SELECT LISTE                      ASSIGN TO W37354D5.                
004300     SELECT LISTF                      ASSIGN TO W37354D6.                
004400     SELECT LISTG                      ASSIGN TO W37354D7.                
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP3                                                                
004800 FILE SECTION.                                                            
004900     SKIP3                                                                
005000 FD  W3734D                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  -COPY W3714D      -L.                                                
005500     SKIP3                                                                
005600 FD  LISTA                                                                
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900     SKIP2                                                                
006000 01  LISTAS                      PIC X(121).                              
006100     SKIP3                                                                
006200 FD  LISTB                                                                
006300     RECORDING       V                                                    
006400     BLOCK CONTAINS  0.                                                   
006500     SKIP2                                                                
006600 01  LISTBS                      PIC X(125).                              
006700     EJECT                                                                
006800     SKIP3                                                                
006900 FD  LISTC                                                                
007000     RECORDING       F                                                    
007100     BLOCK CONTAINS  0.                                                   
007200     SKIP2                                                                
007300 01  LISTCS                      PIC X(121).                              
008000     EJECT                                                                
008100 FD  LISTE                                                                
008200     RECORDING       F                                                    
008300     BLOCK CONTAINS  0.                                                   
008400     SKIP2                                                                
008500 01  LISTES                      PIC X(121).                              
008600 FD  LISTF                                                                
008700     RECORDING       F                                                    
008800     BLOCK CONTAINS  0.                                                   
008900     SKIP2                                                                
009000 01  LISTFS                      PIC X(121).                              
009100 FD  LISTG                                                                
009200     RECORDING       V                                                    
009300     BLOCK CONTAINS  0.                                                   
009400     SKIP2                                                                
009500 01  LISTGS                      PIC X(125).                              
009600     EJECT                                                                
009700 WORKING-STORAGE SECTION.                                                 
009800                                                                          
009900*    -- CHECKED BY WY2000                                                 
010000 77  IDPGM                       PIC X(8)    VALUE 'W3735400'.            
010100 77   PROGRAM-NAMN           VALUE 'W3735400'                             
010200                                 PIC X(8).                                
010300 77  JA                          PIC X       VALUE 'J'.                   
010400 77  NEJ                         PIC X       VALUE 'N'.                   
010500                                                                          
010600 77  WS-SPAR-IDDC                PIC X(2)    VALUE SPACE.                 
010700                                                                          
010800 77  W-KVRETUR11                 PIC S9(7)   VALUE +0 COMP-3.             
010900 77  W-KVRETUR11-W               PIC S9(7)   VALUE +0 COMP-3.             
011000 77  W-KVRETUR91                 PIC S9(7)   VALUE +0 COMP-3.             
011100 77  W-KVRETUR41                 PIC S9(7)   VALUE +0 COMP-3.             
011200 77  W-KVRETUR41-W               PIC S9(7)   VALUE +0 COMP-3.             
011400 77  W-KVRETUR43                 PIC S9(7)   VALUE +0 COMP-3.             
011500 77  W-KVRETUR61                 PIC S9(7)   VALUE +0 COMP-3.             
011600 77  W-KVRETUR61-W               PIC S9(7)   VALUE +0 COMP-3.             
011700 77  W-KVRETURDC                 PIC S9(7)   VALUE +0 COMP-3.             
011800 77  W-KVRETURDC-W               PIC S9(7)   VALUE +0 COMP-3.             
011900 77  W-ANTALPOSTER               PIC S9(7)   VALUE +0 COMP-3.             
012000 77  W-ANTALPOSTER-W             PIC S9(7)   VALUE +0 COMP-3.             
012100 77  W-ANTALPOSTER2              PIC S9(7)   VALUE +0 COMP-3.             
012200 77  W-ANTALPOSTER41             PIC S9(7)   VALUE +0 COMP-3.             
012300 77  W-ANTALPOSTER41-W           PIC S9(7)   VALUE +0 COMP-3.             
012500 77  W-ANTALPOSTER43             PIC S9(7)   VALUE +0 COMP-3.             
012600 77  W-ANTALPOSTER61             PIC S9(7)   VALUE +0 COMP-3.             
012700 77  W-ANTALPOSTER61-W           PIC S9(7)   VALUE +0 COMP-3.             
012800 77  W-ANTALPOSTERDC             PIC S9(7)   VALUE +0 COMP-3.             
012900 77  W-ANTALPOSTERDC-W           PIC S9(7)   VALUE +0 COMP-3.             
013000 77  W-ANTALSUMMA                PIC S9(7)V99 VALUE +0 COMP-3.            
013100 77  W-ANTALSUMMA-W              PIC S9(7)V99 VALUE +0 COMP-3.            
013200 77  W-ANTALSUMMA2               PIC S9(7)V99 VALUE +0 COMP-3.            
013300 77  W-ANTALSUMMA41              PIC S9(7)V99 VALUE +0 COMP-3.            
013400 77  W-ANTALSUMMA41-W            PIC S9(7)V99 VALUE +0 COMP-3.            
013600 77  W-ANTALSUMMA43              PIC S9(7)V99 VALUE +0 COMP-3.            
013700 77  W-ANTALSUMMA61              PIC S9(7)V99 VALUE +0 COMP-3.            
013800 77  W-ANTALSUMMA61-W            PIC S9(7)V99 VALUE +0 COMP-3.            
013900 77  W-ANTALSUMMADC              PIC S9(7)V99 VALUE +0 COMP-3.            
014000 77  W-ANTALSUMMADC-W            PIC S9(7)V99 VALUE +0 COMP-3.            
014100 77  W-KVARBDAG                  PIC S9(7)   VALUE +0 COMP-3.             
014200 77  W-KVARBDAG-W                PIC S9(7)   VALUE +0 COMP-3.             
014300 77  W-KVARBDAG2                 PIC S9(7)   VALUE +0 COMP-3.             
014400 77  W-KVARBDAG2-W               PIC S9(7)   VALUE +0 COMP-3.             
014500 77  W-KVARBDAG41                PIC S9(7)   VALUE +0 COMP-3.             
014600 77  W-KVARBDAG41-W              PIC S9(7)   VALUE +0 COMP-3.             
014800 77  W-KVARBDAG43                PIC S9(7)   VALUE +0 COMP-3.             
014900 77  W-KVARBDAG61                PIC S9(7)   VALUE +0 COMP-3.             
015000 77  W-KVARBDAG61-W              PIC S9(7)   VALUE +0 COMP-3.             
015100 77  W-KVARBDAGDC                PIC S9(7)   VALUE +0 COMP-3.             
015200 77  W-KVARBDAGDC-W              PIC S9(7)   VALUE +0 COMP-3.             
015300                                                                          
015400 77  W3734D-EOF-SW               PIC X       VALUE 'N'.                   
015500     88  END-OF-W3734D                       VALUE 'J'.                   
015600     EJECT                                                                
015700     EJECT                                                                
015800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015900 01  FILLER REDEFINES DAGENS-DATUM.                                       
016000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
016100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
016200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
016300     EJECT                                                                
016400                                                                          
016500 01  WS-DATUM.                                                            
016600     03  DATUM-SEKEL             PIC X(2).                                
016700     03  DATUM-AAR               PIC X(2).                                
016800     03  FILLER                  PIC X(1)    VALUE '-'.                   
016900     03  DATUM-MAANAD            PIC X(2).                                
017000     03  FILLER                  PIC X(1)    VALUE '-'.                   
017100     03  DATUM-DAG               PIC X(2).                                
017200                                                                          
017300                                                                          
017400 01  WS-DATUM-GB.                                                         
017500     03  DATUM-GB-DAG            PIC X(2).                                
017600     03  FILLER                  PIC X(1)    VALUE '-'.                   
017700     03  DATUM-GB-MAANAD         PIC X(2).                                
017800     03  FILLER                  PIC X(1)    VALUE '-'.                   
017900     03  DATUM-GB-SEKEL          PIC X(2).                                
018000     03  DATUM-GB-AAR            PIC X(2).                                
018100                                                                          
018200                                                                          
018300                                                                          
018400 01  WS-DATUM-US.                                                         
018500     03  DATUM-US-MAANAD         PIC X(2).                                
018600     03  FILLER                  PIC X(1)    VALUE '-'.                   
018700     03  DATUM-US-DAG            PIC X(2).                                
018800     03  FILLER                  PIC X(1)    VALUE '-'.                   
018900     03  DATUM-US-SEKEL          PIC X(2).                                
019000     03  DATUM-US-AAR            PIC X(2).                                
019100                                                                          
019200     EJECT                                                                
019300 01  DYNAMISKA-SUBPROGRAM.                                                
019400*                                                                         
019500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
019600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
019700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
019900     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
020000     SKIP2                                                                
020100*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
020200 01  FILLER                      PIC X(16)   VALUE 'DATKORT'.             
020300 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
020400     SKIP2                                                                
020500*01  -COPY WDATKORT                                                       
020600*    --- PARAMETRAR TILL ABEND                                            
020700                                                                          
020800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
020900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021100     SKIP2                                                                
021200 01  FELTEXT.                                                             
021300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
021400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
021500     EJECT                                                                
021600*    --- PARAMETRAR TILL POSTSUM                                          
021700*                                                                         
021800*01  -COPY W0005   -PRE  POSTSUM-                                         
021900     EJECT                                                                
022000*    --- VALID IDDC CODES                                                 
022100*                                                                         
022200*01  -COPY WWDC99                                                         
022300     EJECT                                                                
022400*01  -COPY WDATAREA                                                       
022500     EJECT                                                                
022600*01  -COPY WL10WBDC                                                       
022700     EJECT                                                                
022800 01  IN-AREA-START               PIC X(24)   VALUE                        
022900                                 'IN-AREA-START  '.                       
023000     SKIP2                                                                
023100                                                                          
023200*01  AREA -COPY W3714D     -PRE IN-                                       
023300     EJECT                                                                
023400 01  W001-AREA-START             PIC X(24)   VALUE                        
023500                                 'LIST-AREA-START  '.                     
023600     SKIP2                                                                
023700 01  UT-RAD                      PIC X(121)  VALUE SPACE.                 
023800 01  UT-RAD-91                   PIC X(121)  VALUE SPACE.                 
023900 01  UT-RAD-41                   PIC X(121)  VALUE SPACE.                 
024100 01  UT-RAD-43                   PIC X(121)  VALUE SPACE.                 
024200 01  UT-RAD-61                   PIC X(121)  VALUE SPACE.                 
024300 01  UT-RAD-DC                   PIC X(121)  VALUE SPACE.                 
024400*                                                                         
024500     EJECT                                                                
024600 01  W001R1-RUBRIK-S.                                                     
024700*                                                                         
024800     03  W001R1-STYR           PIC X(1)   VALUE '1'.                      
024900     03  FILLER                PIC X(2)   VALUE SPACE.                    
025000     03  FILLER                PIC X(15)  VALUE 'W37354-011'.             
025100     03  FILLER                PIC X(34)                                  
025200                  VALUE 'SNITTLEDTID FÖR BYTESRETURER FRÅN '.             
025300     03  FILLER                PIC X(39)                                  
025400                  VALUE 'STATUS 3 TILL STATUS 4 PERIOD LISTA'.            
025500     03  FILLER                PIC X(7)   VALUE 'DATUM'.                  
025600     03  W001R1-DAGENS-DATUM   PIC X(10)  VALUE SPACE.                    
025700     03  FILLER                PIC X(5)   VALUE SPACE.                    
025800     03  FILLER                PIC X(7)  VALUE 'SIDA  1'.                 
025900     EJECT                                                                
026000 01  W001R1-RUBRIK-S91.                                                   
026100*                                                                         
026200     03  W001R1-STYR-91        PIC X(1)   VALUE '1'.                      
026300     03  FILLER                PIC X(2)   VALUE SPACE.                    
026400     03  FILLER                PIC X(15)  VALUE 'W37354-091'.             
026500     03  FILLER                PIC X(34)                                  
026600                  VALUE 'AVERAGE LEADTIME ON EXCH.RETURNS. '.             
026700     03  FILLER                PIC X(40)                                  
026800                  VALUE 'FROM STATUS 3 TO 4 PERIOD LIST'.                 
026900     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
027000     03  W001R1-DAGENS-DATUM91 PIC X(10).                                 
027100     03  FILLER                PIC X(5)   VALUE SPACE.                    
027200     03  FILLER                PIC X(7)  VALUE 'PAGE  1'.                 
027300     EJECT                                                                
027400                                                                          
027500 01  W001R1-RUBRIK-S61.                                                   
027600*                                                                         
027700     03  W001R1-STYR-61        PIC X(1)   VALUE '1'.                      
027800     03  FILLER                PIC X(2)   VALUE SPACE.                    
027900     03  FILLER                PIC X(10)  VALUE 'W37354-061'.             
028000     03  FILLER                PIC X(2)   VALUE SPACE.                    
028100     03  FILLER                PIC X(34)                                  
028200                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
028300     03  FILLER                PIC X(40)                                  
028400                  VALUE 'E A CORE PERIOD LIST          '.                 
028500     03  FILLER                PIC X(3)   VALUE SPACE.                    
028600     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
028700     03  W001R1-DAGENS-DATUM61 PIC X(10).                                 
028800     03  FILLER                PIC X(5)   VALUE SPACE.                    
028900     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
029000                                                                          
029100     EJECT                                                                
029200                                                                          
029300 01  W001R1-RUBRIK-DC.                                                    
029400*                                                                         
029500     03  FILLER                PIC X(3)   VALUE SPACE.                    
029600     03  FILLER                PIC X(10)  VALUE 'W37354-001'.             
029700     03  FILLER                PIC X(2)   VALUE SPACE.                    
029800     03  FILLER                PIC X(34)                                  
029900                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
030000     03  FILLER                PIC X(40)                                  
030100                  VALUE 'E A CORE PERIOD LIST          '.                 
030200     03  FILLER                PIC X(3)   VALUE SPACE.                    
030300     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
030400     03  W001R1-DAGENS-DATUMDC PIC X(10).                                 
030500     03  FILLER                PIC X(5)   VALUE SPACE.                    
030600     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
030700                                                                          
030800     EJECT                                                                
030900****  USA RUBRIKER ****                                                   
031000                                                                          
031100 01  W001R1-RUBRIK-S41.                                                   
031200*                                                                         
031300     03  W001R1-STYR-41        PIC X(1)   VALUE '1'.                      
031400     03  FILLER                PIC X(2)   VALUE SPACE.                    
031500     03  FILLER                PIC X(10)  VALUE 'W37354-041'.             
031600     03  FILLER                PIC X(2)   VALUE SPACE.                    
031700     03  FILLER                PIC X(34)                                  
031800                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
031900     03  FILLER                PIC X(40)                                  
032000                  VALUE 'E A CORE PERIOD LIST         '.                  
032100     03  FILLER                PIC X(3)   VALUE SPACE.                    
032200     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
032300     03  W001R1-DAGENS-DATUM41 PIC X(10).                                 
032400     03  FILLER                PIC X(5)   VALUE SPACE.                    
032500     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
032600                                                                          
034200                                                                          
034300 01  W001R1-RUBRIK-S43.                                                   
034400*                                                                         
034500     03  W001R1-STYR-43        PIC X(1)   VALUE '1'.                      
034600     03  FILLER                PIC X(2)   VALUE SPACE.                    
034700     03  FILLER                PIC X(15)  VALUE 'W37354-043'.             
034800     03  FILLER                PIC X(34)                                  
034900                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
035000     03  FILLER                PIC X(40)                                  
035100                  VALUE 'E A CORE PERIOD LIST         '.                  
035200     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
035300     03  W001R1-DAGENS-DATUM43 PIC X(10).                                 
035400     03  FILLER                PIC X(5)   VALUE SPACE.                    
035500     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
035600                                                                          
035700 01  W001R2-DELRUBRIK-1.                                                  
035800*                                                                         
035900     03  W001R2-STYR           PIC X(1)   VALUE '0'.                      
036000     03  FILLER                PIC X(4)   VALUE  SPACE.                   
036100     03  FILLER                PIC X(7)   VALUE 'DC '.                    
036200     03  FILLER                PIC X(6)   VALUE '     '.                  
036300     03  FILLER                PIC X(17)                                  
036400                         VALUE 'SNITT ARBETSTIDEN'.                       
036500     03  FILLER                PIC X(2)   VALUE SPACE.                    
036600     03  FILLER                PIC X(17)                                  
036700                         VALUE 'SNITT ARBETSTIDEN'.                       
036800     03  FILLER                PIC X(17)                                  
036900                         VALUE '  ANTAL ARTIKLAR '.                       
037000     03  FILLER                PIC X(11) VALUE SPACE.                     
037100                                                                          
037200     EJECT                                                                
037300*                                                                         
037400 01  W001R2-DELRUBRIK-3.                                                  
037500     03  FILLER                PIC X(1)   VALUE '0'.                      
037600     03  FILLER                PIC X(4)   VALUE  SPACE.                   
037700     03  FILLER                PIC X(7)   VALUE '   '.                    
037800     03  FILLER                PIC X(6)   VALUE '     '.                  
037900     03  FILLER                PIC X(23)                                  
038000               VALUE '       EJ GARANTI      '.                           
038100     03  FILLER                PIC X(14)                                  
038200               VALUE '      GARANTI '.                                    
038300     03  FILLER                PIC X(2)   VALUE SPACE.                    
038400     03  FILLER                PIC X(4)   VALUE SPACE.                    
038500     03  FILLER                PIC X(21)  VALUE SPACE.                    
038600                                                                          
038700                                                                          
038800                                                                          
038900 01  W001R2-DELRUBRIK-1-91.                                               
039000*                                                                         
039100     03  W001R2-STYR-91        PIC X(1)   VALUE '0'.                      
039200     03  FILLER                PIC X(4)   VALUE  SPACE.                   
039300     03  FILLER                PIC X(7)   VALUE 'DC '.                    
039400     03  FILLER                PIC X(6)   VALUE '     '.                  
039500     03  FILLER                PIC X(32)                                  
039600                VALUE 'WORKTIME AVERAGE  TOTAL WORKTIME'.                 
039700     03  FILLER                PIC X(17)                                  
039800                VALUE '    TOTAL REPORTS'.                                
039900     03  FILLER                PIC X(13)  VALUE SPACE.                    
040000                                                                          
040100     EJECT                                                                
040200                                                                          
040300 01  W001R2-DELRUBRIK-3-61.                                               
040400     03  FILLER                PIC X(1)   VALUE '0'.                      
040500     03  FILLER                PIC X(4)   VALUE  SPACE.                   
040600     03  FILLER                PIC X(7)   VALUE  SPACE.                   
040700     03  FILLER                PIC X(3)   VALUE  SPACE.                   
040800     03  FILLER                PIC X(31)                                  
040900               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
041000     03  FILLER                PIC X(31)                                  
041100               VALUE '           WARRANTY  WARRANTY  '.                   
041200     03  FILLER                PIC X(3)   VALUE SPACE.                    
041300                                                                          
041400     EJECT                                                                
041500                                                                          
041600                                                                          
041700 01  W001R2-DELRUBRIK-3-DC.                                               
041800     03  FILLER                PIC X(5)   VALUE  SPACE.                   
041900     03  FILLER                PIC X(7)   VALUE  SPACE.                   
042000     03  FILLER                PIC X(3)   VALUE  SPACE.                   
042100     03  FILLER                PIC X(31)                                  
042200               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
042300     03  FILLER                PIC X(31)                                  
042400               VALUE '           WARRANTY  WARRANTY  '.                   
042500     03  FILLER                PIC X(3)   VALUE SPACE.                    
042600                                                                          
042700     EJECT                                                                
042800                                                                          
042900 01  W001R2-DELRUBRIK-1-61.                                               
043000*                                                                         
043100     03  W001R2-STYR-61        PIC X(1)   VALUE '0'.                      
043200     03  FILLER                PIC X(4)   VALUE  SPACE.                   
043300     03  FILLER                PIC X(7)   VALUE 'DC '.                    
043400     03  FILLER                PIC X(3)   VALUE '   '.                    
043500     03  FILLER                PIC X(26)                                  
043600               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
043700     03  FILLER                PIC X(8)   VALUE SPACE.                    
043800     03  FILLER                PIC X(26)                                  
043900               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
044000     03  FILLER                PIC X(111)  VALUE SPACE.                   
044100                                                                          
044200                                                                          
044300 01  W001R2-DELRUBRIK-1-DC.                                               
044400*                                                                         
044500     03  FILLER                PIC X(5)   VALUE  SPACE.                   
044600     03  FILLER                PIC X(7)   VALUE 'DC '.                    
044700     03  FILLER                PIC X(3)   VALUE '   '.                    
044800     03  FILLER                PIC X(26)                                  
044900               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
045000     03  FILLER                PIC X(8)   VALUE SPACE.                    
045100     03  FILLER                PIC X(26)                                  
045200               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
045300     03  FILLER                PIC X(111)  VALUE SPACE.                   
045400                                                                          
045500                                                                          
045600*** USA RUBRIKER ****                                                     
045700*                                                                         
045800 01  W001R2-DELRUBRIK-3-41.                                               
045900*                                                                         
046000     03  FILLER                PIC X(1)   VALUE '0'.                      
046100     03  FILLER                PIC X(4)   VALUE  SPACE.                   
046200     03  FILLER                PIC X(7)   VALUE  SPACE.                   
046300     03  FILLER                PIC X(3)   VALUE  SPACE.                   
046400     03  FILLER                PIC X(31)                                  
046500               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
046600     03  FILLER                PIC X(31)                                  
046700               VALUE '           WARRANTY  WARRANTY  '.                   
046800     03  FILLER                PIC X(3)   VALUE SPACE.                    
046900                                                                          
047000                                                                          
047100*** USA RUBRIKER ****                                                     
047200 01  W001R2-DELRUBRIK-1-41.                                               
047300*                                                                         
047400     03  W001R2-STYR-41        PIC X(1)   VALUE '0'.                      
047500     03  FILLER                PIC X(4)   VALUE  SPACE.                   
047600     03  FILLER                PIC X(7)   VALUE 'DC '.                    
047700     03  FILLER                PIC X(3)   VALUE '   '.                    
047800     03  FILLER                PIC X(26)                                  
047900               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
048000     03  FILLER                PIC X(8)   VALUE SPACE.                    
048100     03  FILLER                PIC X(26)                                  
048200               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
048300     03  FILLER                PIC X(111)  VALUE SPACE.                   
048400                                                                          
048500                                                                          
050000                                                                          
050100                                                                          
050200 01  W001R2-DELRUBRIK-1-43.                                               
050300*                                                                         
050400     03  W001R2-STYR-43        PIC X(1)   VALUE '0'.                      
050500     03  FILLER                PIC X(4)   VALUE  SPACE.                   
050600     03  FILLER                PIC X(7)   VALUE 'DC '.                    
050700     03  FILLER                PIC X(6)   VALUE '     '.                  
050800     03  FILLER                PIC X(32)                                  
050900               VALUE 'WORKTIME AVERAGE        QUANTITY'.                  
051000     03  FILLER                PIC X(3)  VALUE SPACE.                     
051100     03  FILLER                PIC X(2)   VALUE SPACE.                    
051200     03  FILLER                PIC X(8)   VALUE SPACE.                    
051300     03  FILLER                PIC X(4)   VALUE SPACE.                    
051400     03  FILLER                PIC X(13)  VALUE SPACE.                    
051500                                                                          
051600     EJECT                                                                
051700                                                                          
051800 01  W001R2-DELRUBRIK-2.                                                  
051900*                                                                         
052000     03  W001R2-STYR           PIC X(1)   VALUE '0'.                      
052100     03  FILLER                PIC X(4)   VALUE  SPACE.                   
052200     03  FILLER                PIC X(2)   VALUE '11'.                     
052300     03  FILLER                PIC X(18)  VALUE SPACE.                    
052400     03  W001R2-ANTAL          PIC Z(7).99.                               
052500     03  FILLER                PIC X(9)  VALUE SPACE.                     
052600     03  W001R2-ANTAL-W        PIC Z(7).99.                               
052700     03  FILLER                PIC X(9)  VALUE SPACE.                     
052800     03  W001R2-KVRETUR-11     PIC Z(6)9.                                 
052900     03  FILLER                PIC X(9)  VALUE SPACE.                     
053000                                                                          
053100     EJECT                                                                
053200*****  BORN RUBRIKER *****                                                
053300 01  W001R2-DELRUBRIK-2-91.                                               
053400*                                                                         
053500     03  W001R2-STYR-91        PIC X(1)   VALUE '0'.                      
053600     03  FILLER                PIC X(4)   VALUE  SPACE.                   
053700     03  FILLER                PIC X(2)   VALUE '91'.                     
053800     03  FILLER                PIC X(17)  VALUE SPACE.                    
053900     03  W001R2-ANTAL-91       PIC Z(7).99.                               
054000     03  FILLER                PIC X(09)  VALUE SPACE.                    
054100     03  W001R2-WORKDAY-91     PIC Z(6)9.                                 
054200     03  FILLER                PIC X(10)  VALUE SPACE.                    
054300     03  W001R2-TOTREPT-91     PIC Z(6)9.                                 
054400                                                                          
054500     EJECT                                                                
054600*****  JAPAN  RUBRIKER *****                                              
054700 01  W001R2-DELRUBRIK-2-61.                                               
054800*                                                                         
054900     03  W001R2-STYR-61        PIC X(1)   VALUE '0'.                      
055000     03  FILLER                PIC X(4)   VALUE  SPACE.                   
055100     03  FILLER                PIC X(2)   VALUE '61'.                     
055200     03  FILLER                PIC X(14)  VALUE SPACE.                    
055300     03  W001R2-ANTAL-61       PIC Z(7).99.                               
055400     03  FILLER                PIC X(8)   VALUE SPACE.                    
055500     03  W001R2-KVRETUR-61     PIC Z(6)9.                                 
055600     03  FILLER                PIC X(9)   VALUE SPACE.                    
055700     03  W001R2-ANTAL-61-W     PIC Z(7).99.                               
055800     03  FILLER                PIC X(3)   VALUE SPACE.                    
055900     03  W001R2-KVRETUR-61-W   PIC Z(6)9.                                 
056000     03  FILLER                PIC X(7)  VALUE SPACE.                     
056100                                                                          
056200*****WEB DC RUBRIK                                                        
056300                                                                          
056400 01  W001R2-DELRUBRIK-2-DC.                                               
056500*                                                                         
056600     03  FILLER                PIC X(5)   VALUE SPACE.                    
056700     03  W001R2-IDDC           PIC X(2)   VALUE SPACE.                    
056800     03  FILLER                PIC X(14)  VALUE SPACE.                    
056900     03  W001R2-ANTAL-DC       PIC Z(7).99.                               
057000     03  FILLER                PIC X(8)   VALUE SPACE.                    
057100     03  W001R2-KVRETUR-DC     PIC Z(6)9.                                 
057200     03  FILLER                PIC X(9)   VALUE SPACE.                    
057300     03  W001R2-ANTAL-DC-W     PIC Z(7).99.                               
057400     03  FILLER                PIC X(3)   VALUE SPACE.                    
057500     03  W001R2-KVRETUR-DC-W   PIC Z(6)9.                                 
057600     03  FILLER                PIC X(7)  VALUE SPACE.                     
057700                                                                          
057800                                                                          
057900 01  W001R2-DELRUBRIK-2-41.                                               
058000*                                                                         
058100     03  W001R2-STYR-41        PIC X(1)   VALUE '0'.                      
058200     03  FILLER                PIC X(4)   VALUE  SPACE.                   
058300     03  FILLER                PIC X(2)   VALUE '41'.                     
058400     03  FILLER                PIC X(14)  VALUE SPACE.                    
058500     03  W001R2-ANTAL-41       PIC Z(7).99.                               
058600     03  FILLER                PIC X(8)   VALUE SPACE.                    
058700     03  W001R2-KVRETUR-41     PIC Z(6)9.                                 
058800     03  FILLER                PIC X(9)   VALUE SPACE.                    
058900     03  W001R2-ANTAL-41-W     PIC Z(7).99.                               
059000     03  FILLER                PIC X(3)   VALUE SPACE.                    
059100     03  W001R2-KVRETUR-41-W   PIC Z(6)9.                                 
059200     03  FILLER                PIC X(7)  VALUE SPACE.                     
059300                                                                          
060700                                                                          
060800 01  W001R2-DELRUBRIK-2-43.                                               
060900*                                                                         
061000     03  W001R2-STYR-43        PIC X(1)   VALUE '0'.                      
061100     03  FILLER                PIC X(4)   VALUE  SPACE.                   
061200     03  FILLER                PIC X(2)   VALUE '43'.                     
061300     03  FILLER                PIC X(17)  VALUE SPACE.                    
061400     03  W001R2-ANTAL-43       PIC Z(7).99.                               
061500     03  FILLER                PIC X(9)   VALUE SPACE.                    
061600     03  W001R2-KVRETUR-43     PIC Z(6)9.                                 
061700     03  FILLER                PIC X(2)   VALUE SPACE.                    
061800     03  FILLER                PIC X(8)   VALUE SPACE.                    
061900     03  FILLER                PIC X(3)   VALUE SPACE.                    
062000     03  FILLER                PIC X(13)  VALUE SPACE.                    
062100     EJECT                                                                
062200                                                                          
062300 01  DAP-CONTROL-REC1.                                                    
062400     03  FILLER                PIC X(15)  VALUE ' ¤DAPW37354-001'.        
062500 01  DAP-CONTROL-REC2.                                                    
062600     03  FILLER                PIC X(05)  VALUE ' ¤DAP'.                  
062700     03  DAP-CONTROL-IDDC      PIC X(02)  VALUE SPACE.                    
062800                                                                          
062900 PROCEDURE DIVISION.                                                      
063000 MAIN SECTION.                                                            
063100     SKIP2                                                                
063200                                                                          
063300     PERFORM A-INIT                                                       
063400     PERFORM S01-LAES-W3734D                                              
063500     PERFORM UNTIL END-OF-W3734D                                          
063600       PERFORM B-BEARBETA                                                 
063700       PERFORM S01-LAES-W3734D                                            
063800     END-PERFORM                                                          
063900                                                                          
064000     PERFORM C-BERAKNA                                                    
064100                                                                          
064200     PERFORM S08-SKRIV-UT-RAD-WEBDC                                       
064300                                                                          
064400     PERFORM Z-FINIT                                                      
064500                                                                          
064600     MOVE ZERO TO RETURN-CODE                                             
064700     GOBACK                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 A-INIT SECTION.                                                          
065100                                                                          
065200*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
065300                                                                          
065400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
065500                                                                          
065600     MOVE   D-AAR            TO  DAGENS-DATUM-AAR                         
065700     MOVE   D-MAANAD         TO  DAGENS-DATUM-MAANAD                      
065800     MOVE   D-DAG            TO  DAGENS-DATUM-DAG                         
065900                                                                          
066000     IF DAGENS-DATUM-AAR > 50                                             
066100        MOVE 19              TO DATUM-SEKEL                               
066200                                DATUM-GB-SEKEL                            
066300                                DATUM-US-SEKEL                            
066400     ELSE                                                                 
066500        MOVE 20              TO DATUM-SEKEL                               
066600                                DATUM-GB-SEKEL                            
066700                                DATUM-US-SEKEL                            
066800     END-IF                                                               
066900                                                                          
067000     MOVE DAGENS-DATUM-AAR    TO DATUM-AAR                                
067100                                 DATUM-GB-AAR                             
067200                                 DATUM-US-AAR                             
067300     MOVE DAGENS-DATUM-MAANAD TO DATUM-MAANAD                             
067400                                 DATUM-GB-MAANAD                          
067500                                 DATUM-US-MAANAD                          
067600     MOVE DAGENS-DATUM-DAG    TO DATUM-DAG                                
067700                                 DATUM-GB-DAG                             
067800                                 DATUM-US-DAG                             
067900                                                                          
068000     MOVE WS-DATUM            TO W001R1-DAGENS-DATUM                      
068100     MOVE WS-DATUM-GB         TO W001R1-DAGENS-DATUM91                    
068200     MOVE WS-DATUM-US         TO W001R1-DAGENS-DATUM41                    
068400                                 W001R1-DAGENS-DATUM43                    
068500                                 W001R1-DAGENS-DATUM61                    
068600                                 W001R1-DAGENS-DATUMDC                    
068700                                                                          
068800                                                                          
068900     OPEN INPUT  W3734D                                                   
069000                                                                          
069100     OPEN OUTPUT LISTA                                                    
069200                 LISTB                                                    
069300                 LISTC                                                    
069500                 LISTE                                                    
069600                 LISTF                                                    
069700                 LISTG                                                    
069800                                                                          
069900***** RUBRIKRAD 1 ******                                                  
070000                                                                          
070100     MOVE W001R1-RUBRIK-S     TO UT-RAD                                   
070200     PERFORM S02-SKRIV-UT-RAD                                             
070300                                                                          
070400     MOVE W001R1-RUBRIK-S91   TO UT-RAD-91                                
070500     PERFORM S03-SKRIV-UT-RAD-91                                          
070600                                                                          
070700     MOVE W001R1-RUBRIK-S41   TO UT-RAD-41                                
070800     PERFORM S04-SKRIV-UT-RAD-41                                          
071200                                                                          
071300     MOVE W001R1-RUBRIK-S43   TO UT-RAD-43                                
071400     PERFORM S06-SKRIV-UT-RAD-43                                          
071500                                                                          
071600     MOVE W001R1-RUBRIK-S61   TO UT-RAD-61                                
071700     PERFORM S07-SKRIV-UT-RAD-61                                          
071800                                                                          
071900                                                                          
072000***** RUBRIKRAD 2 ******                                                  
072100                                                                          
072200     MOVE W001R2-DELRUBRIK-1  TO UT-RAD                                   
072300     PERFORM S02-SKRIV-UT-RAD                                             
072400                                                                          
072500     MOVE W001R2-DELRUBRIK-3  TO UT-RAD                                   
072600     PERFORM S02-SKRIV-UT-RAD                                             
072700                                                                          
072800     MOVE W001R2-DELRUBRIK-1-91  TO UT-RAD-91                             
072900     PERFORM S03-SKRIV-UT-RAD-91                                          
073000                                                                          
073100     MOVE W001R2-DELRUBRIK-1-41  TO UT-RAD-41                             
073200     PERFORM S04-SKRIV-UT-RAD-41                                          
073300                                                                          
073400     MOVE W001R2-DELRUBRIK-3-41  TO UT-RAD-41                             
073500     PERFORM S04-SKRIV-UT-RAD-41                                          
073900                                                                          
074000     MOVE W001R2-DELRUBRIK-1-43  TO UT-RAD-43                             
074100     PERFORM S06-SKRIV-UT-RAD-43                                          
074200                                                                          
074300     MOVE W001R2-DELRUBRIK-1-61  TO UT-RAD-61                             
074400     PERFORM S07-SKRIV-UT-RAD-61                                          
074500                                                                          
074600     MOVE W001R2-DELRUBRIK-3-61  TO UT-RAD-61                             
074700     PERFORM S07-SKRIV-UT-RAD-61                                          
074800                                                                          
074900     SKIP2                                                                
075000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
075100                                                                          
075200     .                                                                    
075300     EJECT                                                                
075400 B-BEARBETA SECTION.                                                      
075500                                                                          
075600     MOVE IN-IDDC              TO WS-IDDC                                 
075700                                  WBDC-IDDC                               
075800                                                                          
075900     CALL WL10WBDC            USING WBDC-AREA                             
076000                                                                          
076400     IF WBDC-FLWEBDC = 'J'                                                
076800       PERFORM BA-PROCESS-WEBDC                                           
076900     ELSE                                                                 
077000       EVALUATE TRUE                                                      
077100         WHEN CDC-SE                                                      
077200           IF IN-FLBYTGAR = 'J'                                           
077300             ADD +1            TO W-ANTALPOSTER-W                         
077400             ADD IN-KVARBDAG   TO W-KVARBDAG-W                            
077500             ADD IN-KVRETUR    TO W-KVRETUR11                             
077600           ELSE                                                           
077700             ADD +1            TO W-ANTALPOSTER                           
077800             ADD IN-KVARBDAG   TO W-KVARBDAG                              
077900             ADD IN-KVRETUR    TO W-KVRETUR11                             
078000           END-IF                                                         
078100         WHEN SDC-NL-ET                                                   
078200           ADD +1            TO W-ANTALPOSTER2                            
078300           ADD IN-KVARBDAG   TO W-KVARBDAG2                               
078400           ADD IN-KVRETUR    TO W-KVRETUR91                               
078500         WHEN NDC-US-RU                                                   
078600           IF IN-FLBYTGAR = 'J'                                           
078700              ADD +1            TO W-ANTALPOSTER41-W                      
078800              ADD IN-KVARBDAG   TO W-KVARBDAG41-W                         
078900              ADD IN-KVRETUR    TO W-KVRETUR41-W                          
079000           ELSE                                                           
079100              ADD +1            TO W-ANTALPOSTER41                        
079200              ADD IN-KVARBDAG   TO W-KVARBDAG41                           
079300              ADD IN-KVRETUR    TO W-KVRETUR41                            
079400           END-IF                                                         
079900         WHEN NDC-US-LA                                                   
080000           ADD +1            TO W-ANTALPOSTER43                           
080100           ADD IN-KVARBDAG   TO W-KVARBDAG43                              
080200           ADD IN-KVRETUR    TO W-KVRETUR43                               
080300         WHEN NDC-JP                                                      
080400           IF IN-FLBYTGAR = 'J'                                           
080500              ADD +1            TO W-ANTALPOSTER61-W                      
080600              ADD IN-KVARBDAG   TO W-KVARBDAG61-W                         
080700              ADD IN-KVRETUR    TO W-KVRETUR61-W                          
080800           ELSE                                                           
080900              ADD +1            TO W-ANTALPOSTER61                        
081000              ADD IN-KVARBDAG   TO W-KVARBDAG61                           
081100              ADD IN-KVRETUR    TO W-KVRETUR61                            
081200           END-IF                                                         
081300       END-EVALUATE                                                       
081400     END-IF                                                               
081500                                                                          
081600     .                                                                    
081700     EJECT                                                                
081800                                                                          
081900 BA-PROCESS-WEBDC SECTION.                                                
082000                                                                          
082100     IF IN-IDDC = WS-SPAR-IDDC                                            
082200       IF IN-FLBYTGAR = 'J'                                               
082300         ADD +1                  TO W-ANTALPOSTERDC-W                     
082400         ADD IN-KVARBDAG         TO W-KVARBDAGDC-W                        
082500         ADD IN-KVRETUR          TO W-KVRETURDC-W                         
082600       ELSE                                                               
082700         ADD +1                  TO W-ANTALPOSTERDC                       
082800         ADD IN-KVARBDAG         TO W-KVARBDAGDC                          
082900         ADD IN-KVRETUR          TO W-KVRETURDC                           
083000       END-IF                                                             
083100     ELSE                                                                 
083200       PERFORM S08-SKRIV-UT-RAD-WEBDC                                     
083300       MOVE IN-IDDC              TO WS-SPAR-IDDC                          
083400       MOVE ZERO                 TO W-ANTALPOSTERDC-W                     
083500                                    W-KVARBDAGDC-W                        
083600                                    W-KVRETURDC-W                         
083700                                    W-ANTALPOSTERDC                       
083800                                    W-KVARBDAGDC                          
083900                                    W-KVRETURDC                           
084000                                                                          
084100       IF IN-FLBYTGAR = 'J'                                               
084200         ADD +1                  TO W-ANTALPOSTERDC-W                     
084300         ADD IN-KVARBDAG         TO W-KVARBDAGDC-W                        
084400         ADD IN-KVRETUR          TO W-KVRETURDC-W                         
084500       ELSE                                                               
084600         ADD +1                  TO W-ANTALPOSTERDC                       
084700         ADD IN-KVARBDAG         TO W-KVARBDAGDC                          
084800         ADD IN-KVRETUR          TO W-KVRETURDC                           
084900       END-IF                                                             
085000     END-IF                                                               
085100                                                                          
085200     .                                                                    
085300     EJECT                                                                
085400                                                                          
085500 C-BERAKNA SECTION.                                                       
085600                                                                          
085700     IF W-ANTALPOSTER > ZERO                                              
085800        COMPUTE W-ANTALSUMMA  =  W-KVARBDAG /  W-ANTALPOSTER              
085900     END-IF                                                               
086000     IF W-ANTALPOSTER-W > ZERO                                            
086100        COMPUTE W-ANTALSUMMA-W = W-KVARBDAG-W / W-ANTALPOSTER-W           
086200     END-IF                                                               
086300     IF W-ANTALPOSTER2 > ZERO                                             
086400        COMPUTE W-ANTALSUMMA2 =  W-KVARBDAG2 / W-ANTALPOSTER2             
086500     END-IF                                                               
086600     IF W-ANTALPOSTER41 > ZERO                                            
086700        COMPUTE W-ANTALSUMMA41 =  W-KVARBDAG41 / W-ANTALPOSTER41          
086800     END-IF                                                               
086900     IF W-ANTALPOSTER41-W > ZERO                                          
087000        COMPUTE W-ANTALSUMMA41-W =                                        
087100                        W-KVARBDAG41-W / W-ANTALPOSTER41-W                
087200     END-IF                                                               
087600     IF W-ANTALPOSTER43 > ZERO                                            
087700        COMPUTE W-ANTALSUMMA43 =  W-KVARBDAG43 / W-ANTALPOSTER43          
087800     END-IF                                                               
087900     IF W-ANTALPOSTER61 > ZERO                                            
088000        COMPUTE W-ANTALSUMMA61 =  W-KVARBDAG61 / W-ANTALPOSTER61          
088100     END-IF                                                               
088200     IF W-ANTALPOSTER61-W > ZERO                                          
088300        COMPUTE W-ANTALSUMMA61-W =                                        
088400                        W-KVARBDAG61-W / W-ANTALPOSTER61-W                
088500     END-IF                                                               
088600                                                                          
088700     MOVE W-ANTALSUMMA       TO W001R2-ANTAL                              
088800     MOVE W-ANTALSUMMA-W     TO W001R2-ANTAL-W                            
088900     MOVE W-ANTALSUMMA2      TO W001R2-ANTAL-91                           
089000     MOVE W-ANTALSUMMA41     TO W001R2-ANTAL-41                           
089100     MOVE W-ANTALSUMMA41-W   TO W001R2-ANTAL-41-W                         
089300     MOVE W-ANTALSUMMA43     TO W001R2-ANTAL-43                           
089400     MOVE W-ANTALSUMMA61     TO W001R2-ANTAL-61                           
089500     MOVE W-ANTALSUMMA61-W   TO W001R2-ANTAL-61-W                         
089600     MOVE W-KVRETUR11        TO W001R2-KVRETUR-11                         
089700     MOVE W-KVARBDAG2        TO W001R2-WORKDAY-91                         
089800     MOVE W-ANTALPOSTER2     TO W001R2-TOTREPT-91                         
089900     MOVE W-KVRETUR41        TO W001R2-KVRETUR-41                         
090000     MOVE W-KVRETUR41-W      TO W001R2-KVRETUR-41-W                       
090200     MOVE W-KVRETUR43        TO W001R2-KVRETUR-43                         
090300     MOVE W-KVRETUR61        TO W001R2-KVRETUR-61                         
090400     MOVE W-KVRETUR61-W      TO W001R2-KVRETUR-61-W                       
090500                                                                          
090600                                                                          
090700***** RUBRIKRAD 3 ******                                                  
090800                                                                          
090900     MOVE W001R2-DELRUBRIK-2 TO UT-RAD                                    
091000     PERFORM S02-SKRIV-UT-RAD                                             
091100                                                                          
091200     MOVE W001R2-DELRUBRIK-2-91 TO UT-RAD-91                              
091300     PERFORM S03-SKRIV-UT-RAD-91                                          
091400                                                                          
091500     MOVE W001R2-DELRUBRIK-2-41 TO UT-RAD-41                              
091600     PERFORM S04-SKRIV-UT-RAD-41                                          
092000                                                                          
092100     MOVE W001R2-DELRUBRIK-2-43 TO UT-RAD-43                              
092200     PERFORM S06-SKRIV-UT-RAD-43                                          
092300                                                                          
092400     MOVE W001R2-DELRUBRIK-2-61 TO UT-RAD-61                              
092500     PERFORM S07-SKRIV-UT-RAD-61                                          
092600                                                                          
092700     .                                                                    
092800     EJECT                                                                
092900 Z-FINIT SECTION.                                                         
093000     CLOSE W3734D                                                         
093100           LISTA                                                          
093200           LISTB                                                          
093300           LISTC                                                          
093500           LISTE                                                          
093600           LISTF                                                          
093700           LISTG                                                          
093800     SKIP2                                                                
093900     MOVE 'S' TO POSTSUM-OPKOD                                            
094000     CALL POSTSUM USING POSTSUM-PARM                                      
094100     .                                                                    
094200     EJECT                                                                
094300 S01-LAES-W3734D  SECTION.                                                
094400     READ W3734D INTO IN-AREA                                             
094500     AT END                                                               
094600        MOVE HIGH-VALUE TO IN-AREA                                        
094700        SET END-OF-W3734D TO TRUE                                         
094800                                                                          
094900     NOT AT END                                                           
095000        MOVE 'W3734D' TO POSTSUM-FDNAMN                                   
095100        MOVE 'W37354D1' TO POSTSUM-DDNAMN2                                
095200        MOVE ' UT '    TO POSTSUM-TRANSTYP                                
095300        CALL POSTSUM USING POSTSUM-PARM                                   
095400     END-READ                                                             
095500     .                                                                    
095600     EJECT                                                                
095700 S02-SKRIV-UT-RAD SECTION.                                                
095800     SKIP2                                                                
095900                                                                          
096000     WRITE LISTAS FROM UT-RAD                                             
096100                                                                          
096200     .                                                                    
096300     EJECT                                                                
096400 S03-SKRIV-UT-RAD-91 SECTION.                                             
096500     SKIP2                                                                
096600                                                                          
096700     WRITE LISTBS FROM UT-RAD-91                                          
096800                                                                          
096900     .                                                                    
097000     EJECT                                                                
097100 S04-SKRIV-UT-RAD-41 SECTION.                                             
097200     SKIP2                                                                
097300                                                                          
097400     WRITE LISTCS FROM UT-RAD-41                                          
097500                                                                          
097600     .                                                                    
098400     EJECT                                                                
098500 S06-SKRIV-UT-RAD-43 SECTION.                                             
098600     SKIP2                                                                
098700                                                                          
098800     WRITE LISTES FROM UT-RAD-43                                          
098900                                                                          
099000     .                                                                    
099100     EJECT                                                                
099200 S07-SKRIV-UT-RAD-61 SECTION.                                             
099300     SKIP2                                                                
099400                                                                          
099500     WRITE LISTFS FROM UT-RAD-61                                          
099600                                                                          
099700     .                                                                    
099800     EJECT                                                                
099900 S08-SKRIV-UT-RAD-WEBDC SECTION.                                          
100000                                                                          
100100     MOVE WS-SPAR-IDDC           TO WBDC-IDDC                             
100200                                    WS-IDDC                               
100300                                                                          
100400     CALL WL10WBDC            USING WBDC-AREA                             
100500                                                                          
100700     IF WBDC-FLWEBDC = 'J'                                                
101100                                                                          
101200*      DAP CONTROL RECORDS                                                
101300       MOVE DAP-CONTROL-REC1     TO UT-RAD-DC                             
101400       WRITE LISTGS            FROM UT-RAD-DC                             
101500                                                                          
101600       MOVE WS-SPAR-IDDC         TO DAP-CONTROL-IDDC                      
101700       MOVE DAP-CONTROL-REC2     TO UT-RAD-DC                             
101800       WRITE LISTGS            FROM UT-RAD-DC                             
101900                                                                          
102000*      REPORT HEADER 1                                                    
102100       MOVE W001R1-RUBRIK-DC     TO UT-RAD-DC                             
102200       WRITE LISTGS            FROM UT-RAD-DC                             
102300                                                                          
102400*      REPORT HEADER 2                                                    
102500       MOVE W001R2-DELRUBRIK-1-DC                                         
102600                                 TO UT-RAD-DC                             
102700       WRITE LISTGS            FROM UT-RAD-DC                             
102800                                                                          
102900*      REPORT HEADER 3                                                    
103000       MOVE W001R2-DELRUBRIK-3-DC                                         
103100                                 TO UT-RAD-DC                             
103200       WRITE LISTGS            FROM UT-RAD-DC                             
103300                                                                          
103400*      REPORT DATA                                                        
103500       IF W-ANTALPOSTERDC > ZERO                                          
103600         COMPUTE W-ANTALSUMMADC =                                         
103700                        W-KVARBDAGDC / W-ANTALPOSTERDC                    
103800       END-IF                                                             
103900       IF W-ANTALPOSTERDC-W > ZERO                                        
104000         COMPUTE W-ANTALSUMMADC-W =                                       
104100                        W-KVARBDAGDC-W / W-ANTALPOSTERDC-W                
104200       END-IF                                                             
104300                                                                          
104400       MOVE WS-SPAR-IDDC         TO W001R2-IDDC                           
104500       MOVE W-ANTALSUMMADC       TO W001R2-ANTAL-DC                       
104600       MOVE W-ANTALSUMMADC-W     TO W001R2-ANTAL-DC-W                     
104700       MOVE W-KVRETURDC          TO W001R2-KVRETUR-DC                     
104800       MOVE W-KVRETURDC-W        TO W001R2-KVRETUR-DC-W                   
104900                                                                          
105000       MOVE W001R2-DELRUBRIK-2-DC                                         
105100                                 TO UT-RAD-DC                             
105200       WRITE LISTGS            FROM UT-RAD-DC                             
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 S99-ABEND SECTION.                                                       
105700                                                                          
105800     SKIP2                                                                
105900     MOVE 'S' TO POSTSUM-OPKOD                                            
106000     CALL POSTSUM USING POSTSUM-PARM                                      
106100     CALL ABEND USING RKOD-ABEND                                          
106200     .                                                                    
