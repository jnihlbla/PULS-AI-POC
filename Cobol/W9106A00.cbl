000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9106A00.                                                
000300 AUTHOR.         STEFAN KIHLBERG                                          
000400 DATE-WRITTEN.   DEC 2013.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THE PGM                                                          
000900*        - READS FILE WITH INFO ABOUT NEW AND CHANGED PARTS               
001000*        - SENDS DOCUMENT DATA RECORDS FOR LOGENT (EXCEL)                 
001100*        - TEMPORARY SOLUTION                                             
001200*          TO DISTRIBUTION & PRINT BY USING WZ01SEND                      
001300*                                                                         
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600                                                                          
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000*          --- INFIL                                                      
002100     SELECT W91061                     ASSIGN TO W9106AD1.                
002200*          --- UTFIL                                                      
002300     SELECT W9106A                     ASSIGN TO W9106AD2.                
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 FILE SECTION.                                                            
002800 FD  W91061                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY A7290B01   -L                                                  
003300                                                                          
003400     EJECT                                                                
003500 FD  W9106A                                                               
003600     RECORDING       V                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900 01  UT-POST       PIC X(150).                                            
004000     EJECT                                                                
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W9106A00'.            
004500 77  WS-ADRESS                   PIC X(50)                                
004600         VALUE 'CARPARTS.DAP.NEWSUPPLIERLOGENT'.                          
004700 77  KDRC-DISPLAY                PIC Z(5).                                
004800 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005500                                                                          
005600 77  W91061-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W91061                       VALUE 'J'.                   
005800                                                                          
005900                                                                          
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500                                                                          
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007200                                                                          
007300 01  FELTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007600     EJECT                                                                
007700                                                                          
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100                                                                          
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                 'UT-AREA-START  '.                       
008400*01  AREA -COPY A7290B01   -PRE IN-                                       
008500     EJECT                                                                
008600                                                                          
008700 01  UT-AREA-START           PIC X(24)   VALUE                            
008800                                 'UT-AREA-START '.                        
008900 01  UT-AREA                 PIC X(150).                                  
009000     EJECT                                                                
009100                                                                          
009200 PROCEDURE DIVISION.                                                      
009300                                                                          
009400 MAIN SECTION.                                                            
009500                                                                          
009600     PERFORM A-INIT                                                       
009700                                                                          
009800     PERFORM S11-READ-W91061                                              
009900     PERFORM UNTIL END-OF-W91061                                          
009910       IF IN-POSTTYP = '11'                                               
009920         PERFORM B-CREATE-PUT-HEAD-LINE-1                                 
009930         PERFORM S12-WRITE-W9106A                                         
009940         PERFORM B-CREATE-PUT-HEAD-LINE-2                                 
009950       ELSE                                                               
009960         PERFORM C-CREATE-PUT-LINE                                        
009970                                                                          
009980       END-IF                                                             
009990       PERFORM S12-WRITE-W9106A                                           
009991       PERFORM S11-READ-W91061                                            
010800     END-PERFORM                                                          
010900                                                                          
011000                                                                          
011100     PERFORM Z-FINIT                                                      
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     .                                                                    
011500     EJECT                                                                
011600                                                                          
011700 A-INIT SECTION.                                                          
011800     OPEN INPUT W91061                                                    
011900     OPEN OUTPUT W9106A                                                   
012000     ACCEPT DAGENS-DATUM FROM DATE                                        
012100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012200     .                                                                    
012300     EJECT                                                                
012310 B-CREATE-PUT-HEAD-LINE-1.                                                
012320                                                                          
012330     MOVE SPACE TO UT-AREA                                                
012340                                                                          
012350     STRING 'PULS  W910 NEW / CHANGED SUPPLIER'   ';'                     
012351      'DEMAND FOR COUNTRY OF ORIGEN'   ';'                                
012360     DELIMITED BY SIZE INTO UT-AREA                                       
012380     .                                                                    
012390     EJECT                                                                
012391                                                                          
012392 B-CREATE-PUT-HEAD-LINE-2.                                                
012393                                                                          
012394     MOVE SPACE TO UT-AREA                                                
012395                                                                          
012396     STRING 'PART NUMBER'  ';'                                            
012397       'ACTION CODE'       ';'                                            
012398       'SUPPLIER NO'       ';'                                            
012399       'DISCR SWE'         ';'                                            
012400       'DISCR GB'          ';'                                            
012401*      WS-NETTOVIKT        ';'                                            
012402       'U O M'             ';'                                            
012403       'PULS COMM CODE'    ';'                                            
012404       'LOGENT COMM CODE'  ';'                                            
012405       'PULS C O ORIGIN'   ';'                                            
012406       'LOGENT C O ORIGIN' ';'                                            
012407     DELIMITED BY SIZE INTO UT-AREA                                       
012409     .                                                                    
012410     EJECT                                                                
012411                                                                          
012420                                                                          
012500 C-CREATE-PUT-LINE SECTION.                                               
012600                                                                          
012700     MOVE SPACE TO UT-AREA                                                
012800                                                                          
012900     STRING IN-ARTIKELNR  ';'                                             
013000       IN-ATGARDSKOD      ';'                                             
013100       IN-LEVID           ';'                                             
013200       IN-BENAMNING-SE    ';'                                             
013300       IN-BENAMNING-GB    ';'                                             
013400       IN-ANTALSTYP       ';'                                             
013500       IN-STATNR          ';' ';'                                         
013600       IN-ARTIKELURSPRUNG ';' ';'                                         
013700     DELIMITED BY SIZE INTO UT-AREA                                       
013800     .                                                                    
013900     EJECT                                                                
014000                                                                          
014100 Z-FINIT SECTION.                                                         
014200     CLOSE W91061 W9106A                                                  
014300                                                                          
014400     MOVE 'S' TO POSTSUM-OPKOD                                            
014500     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014700     EJECT                                                                
014800                                                                          
014900 S11-READ-W91061 SECTION.                                                 
015000     READ W91061 INTO IN-AREA                                             
015100     AT END                                                               
015200       SET END-OF-W91061 TO TRUE                                          
015300                                                                          
015400     NOT AT END                                                           
015500       MOVE 'W91961'   TO POSTSUM-FDNAMN                                  
015600       MOVE 'W9106AD1' TO POSTSUM-DDNAMN2                                 
015700       MOVE SPACE      TO POSTSUM-TRANSTYP                                
015800       CALL POSTSUM USING POSTSUM-PARM                                    
015900     END-READ                                                             
016000     .                                                                    
016100     EJECT                                                                
016200                                                                          
016300 S12-WRITE-W9106A SECTION.                                                
016400                                                                          
016500     WRITE UT-POST FROM UT-AREA                                           
016600                                                                          
016700     MOVE 'W9106A'   TO POSTSUM-FDNAMN                                    
016800     MOVE 'W9106AD2' TO POSTSUM-DDNAMN2                                   
016900     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
017100     EJECT                                                                
017200                                                                          
