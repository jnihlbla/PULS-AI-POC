000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1117A00.                                                
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
002100     SELECT W11175                     ASSIGN TO W1117AD1.                
002200*          --- UTFIL                                                      
002300     SELECT W1117A                     ASSIGN TO W1117AD2.                
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 FILE SECTION.                                                            
002800 FD  W11175                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY A7290B01     -L.                                               
003300                                                                          
003400     EJECT                                                                
003500 FD  W1117A                                                               
003600     RECORDING       V                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900 01  UT-POST       PIC X(150).                                            
004000     EJECT                                                                
004100                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W1117A00'.            
004500 77  WS-ADRESS                   PIC X(50)                                
004600         VALUE 'CARPARTS.DAP.NEWPARTSLOGENT'.                             
004700 77  KDRC-DISPLAY                PIC Z(5).                                
004800 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005500                                                                          
005600 77  W11175-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W11175                       VALUE 'J'.                   
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
007110     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007200                                                                          
007300 01  FELTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007600     EJECT                                                                
007700                                                                          
007701*    --- PARAMETRAR TILL DECEDIT                                          
007710*01  -COPY WDECAREA                                                       
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000     EJECT                                                                
008100                                                                          
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                 'UT-AREA-START  '.                       
008400*01  AREA -COPY A7290B01   -PRE IN-                                       
008500     EJECT                                                                
008900                                                                          
009500 01  UT-AREA-START           PIC X(24)   VALUE                            
009600                                 'UT-AREA-START '.                        
009700 01  UT-AREA                 PIC X(150).                                  
009800     EJECT                                                                
009900                                                                          
010000 PROCEDURE DIVISION.                                                      
010100                                                                          
010200 MAIN SECTION.                                                            
010300                                                                          
010400     PERFORM A-INIT                                                       
010500                                                                          
010600     PERFORM S11-READ-W11175                                              
010700     PERFORM UNTIL END-OF-W11175                                          
010800       IF IN-POSTTYP = '11'                                               
010900         PERFORM B-CREATE-PUT-HEAD-LINE-1                                 
010902         PERFORM S12-WRITE-W1117A                                         
010910         PERFORM B-CREATE-PUT-HEAD-LINE-2                                 
011000       ELSE                                                               
011100         PERFORM C-CREATE-PUT-LINE                                        
011200                                                                          
011300       END-IF                                                             
011310     PERFORM S12-WRITE-W1117A                                             
011400     PERFORM S11-READ-W11175                                              
011500     END-PERFORM                                                          
011600                                                                          
011700                                                                          
011800     PERFORM Z-FINIT                                                      
011900     MOVE ZERO TO RETURN-CODE                                             
012000     GOBACK                                                               
012100     .                                                                    
012200     EJECT                                                                
012300                                                                          
012400 A-INIT SECTION.                                                          
012500     OPEN INPUT W11175                                                    
012600     OPEN OUTPUT W1117A                                                   
012700     ACCEPT DAGENS-DATUM FROM DATE                                        
012800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012900     .                                                                    
013000     EJECT                                                                
013100                                                                          
013101 B-CREATE-PUT-HEAD-LINE-1.                                                
013102                                                                          
013103     MOVE SPACE TO UT-AREA                                                
013104                                                                          
013105     STRING 'PULS  W111 NEW / CHANGED PARTS'   ';'                        
013106      'DEMAND FOR COMMODITY CODE'   ';'                                   
013116     DELIMITED BY SIZE INTO UT-AREA                                       
013118     .                                                                    
013119     EJECT                                                                
013120                                                                          
013121 B-CREATE-PUT-HEAD-LINE-2.                                                
013122                                                                          
013123     MOVE SPACE TO UT-AREA                                                
013124                                                                          
013125     STRING 'PART NUMBER'  ';'                                            
013126       'ACTION CODE'       ';'                                            
013127       'SUPPLIER NO'       ';'                                            
013128       'DISCR SWE'         ';'                                            
013129       'DISCR GB'          ';'                                            
013130*      WS-NETTOVIKT        ';'                                            
013131       'U O M'             ';'                                            
013132       'PULS COMM CODE'    ';'                                            
013133       'LOGENT COMM CODE'  ';'                                            
013134       'PULS C O ORIGIN'   ';'                                            
013135       'LOGENT C O ORIGIN' ';'                                            
013136     DELIMITED BY SIZE INTO UT-AREA                                       
013138     .                                                                    
013139     EJECT                                                                
013140                                                                          
013200 C-CREATE-PUT-LINE SECTION.                                               
013300                                                                          
013301*    MOVE ZERO                  TO WS-NETTOVIKT                           
013310*    INSPECT IN-NETTOVIKT REPLACING                                       
013320*           LEADING SPACE BY ZERO                                         
013330*    MOVE IN-NETTOVIKT          TO DEC-IDFRIDATA                          
013340*    MOVE 7                     TO DEC-KVHELTAL                           
013350*    MOVE 3                     TO DEC-KVDECIMAL                          
013360*    CALL WDECEDIT USING DEC-WDECAREA                                     
013370*                                                                         
013380*    IF DEC-KDSVAR-OK AND DEC-IDEDITDATA > ZERO                           
013390*       MOVE DEC-IDEDITDATA  TO  WS-NETTOVIKT                             
013392*    ELSE                                                                 
013393*       MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                          
013394*                               RESP-IDMSG-ERROR-RAD (RADIND)             
013395*       MOVE 'NETTOVIKT'     TO RESP-IDELMT-ERROR                         
013396*       MOVE NEJ             TO INDATA-SW                                 
013397*    END-IF                                                               
013398*    INSPECT WS-NETTOVIKT REPLACING                                       
013399*           LEADING SPACE BY ZERO                                         
013401*                                                                         
013410     MOVE SPACE TO UT-AREA                                                
013500                                                                          
013600     STRING IN-ARTIKELNR  ';'                                             
013700       IN-ATGARDSKOD      ';'                                             
013800       IN-LEVID           ';'                                             
013900       IN-BENAMNING-SE    ';'                                             
014000       IN-BENAMNING-GB    ';'                                             
014100*      WS-NETTOVIKT       ';'                                             
014200       IN-ANTALSTYP       ';'                                             
014300       IN-STATNR          ';' ';'                                         
014400       IN-ARTIKELURSPRUNG ';' ';'                                         
014500     DELIMITED BY SIZE INTO UT-AREA                                       
014600     .                                                                    
014700     EJECT                                                                
014800                                                                          
014900 Z-FINIT SECTION.                                                         
015000     CLOSE W11175 W1117A                                                  
015100                                                                          
015200     MOVE 'S' TO POSTSUM-OPKOD                                            
015300     CALL POSTSUM USING POSTSUM-PARM                                      
015400     .                                                                    
015500     EJECT                                                                
015600                                                                          
015700 S11-READ-W11175 SECTION.                                                 
015800     READ W11175 INTO IN-AREA                                             
015900     AT END                                                               
016000       SET END-OF-W11175 TO TRUE                                          
016100                                                                          
016200     NOT AT END                                                           
016300       MOVE 'W11175'   TO POSTSUM-FDNAMN                                  
016400       MOVE 'W1117AD1' TO POSTSUM-DDNAMN2                                 
016500       MOVE SPACE      TO POSTSUM-TRANSTYP                                
016600       CALL POSTSUM USING POSTSUM-PARM                                    
016700     END-READ                                                             
016800     .                                                                    
016900     EJECT                                                                
017000                                                                          
017100 S12-WRITE-W1117A SECTION.                                                
017200                                                                          
017300     WRITE UT-POST FROM UT-AREA                                           
017400                                                                          
017500     MOVE 'W1117A'   TO POSTSUM-FDNAMN                                    
017600     MOVE 'W1117AD2' TO POSTSUM-DDNAMN2                                   
017700     CALL POSTSUM USING POSTSUM-PARM                                      
017800     .                                                                    
017900     EJECT                                                                
018000                                                                          
