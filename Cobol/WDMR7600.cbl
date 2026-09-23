000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WDMR7600.                                                
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
002500*          --- LOGSEGM - FYSSEGM - KOPPLINGAR                             
002600     SELECT WDMR74                     ASSIGN TO WDMR76D1.                
002700     SKIP2                                                                
002800*          --- DMRLADD-DATA                                               
002900     SELECT WDMR76                     ASSIGN TO WDMR76D2.                
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000     SKIP3                                                                
005100 FILE SECTION.                                                            
005200     SKIP3                                                                
005300 FD  WDMR74                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS 0.                                                    
005600     SKIP2                                                                
005700 01  FILLER                   PIC X(37).                                  
005800     SKIP3                                                                
007700 FD  WDMR76                                                               
007800     RECORDING       F                                                    
007900     BLOCK CONTAINS 0.                                                    
008000     SKIP2                                                                
008100 01  LSEG-POST                PIC X(80).                                  
010000     EJECT                                                                
010100 WORKING-STORAGE SECTION.                                                 
010200     SKIP2                                                                
010201                                                                          
010210*    -- CHECKED BY WY2000                                                 
010300 77  IDPGM                       PIC X(8)    VALUE 'WDMR7600'.            
010400 77  JA                          PIC X       VALUE 'J'.                   
010500 77  NEJ                         PIC X       VALUE 'N'.                   
010600 77  GROUP-SEGM-CAT              PIC X(20)                                
010601                           VALUE '''GROUP'',''SEGM'''.                    
010700 77  W-STR                       PIC X(200).                              
010710 77  W-CAT-WORD                  PIC X(10).                               
010800 77  END-PUNKT                   PIC X       VALUE 'N'.                   
011700     EJECT                                                                
011800* EOF FLAGGOR                                                             
011900                                                                          
012000 77  WDMR74-EOF-SW               PIC X       VALUE 'N'.                   
012100     88  END-OF-WDMR74                       VALUE 'J'.                   
013100     EJECT                                                                
013200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013300 01  FILLER REDEFINES DAGENS-DATUM.                                       
013400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013700     SKIP3                                                                
013800 01  DYNAMISKA-SUBPROGRAM.                                                
013900*                                                                         
014000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
014100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014200                                                                          
014300*    --- PARAMETRAR TILL ABEND                                            
014400                                                                          
014500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014700     SKIP2                                                                
014800 01  FELTEXT.                                                             
014900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015100     EJECT                                                                
015200*    --- ARBETSAREOR FÖR REDIGERING AV UTFILER                            
015300 01  REPLACE-COMMAND.                                                     
015400     03                          PIC X(08) VALUE 'REPLACE'.               
015500     03 REPL-MEMBER              PIC X(50).                               
015600     03                          PIC X(01) VALUE '.'.                     
015601     SKIP2                                                                
015610 01  REMOVE-COMMAND.                                                      
015620     03                          PIC X(08) VALUE 'REMOVE'.                
015630     03 REM-MEMBER               PIC X(50).                               
015640     03                          PIC X(01) VALUE '.'.                     
015650     SKIP2                                                                
015700 01  RUBRIKER.                                                            
015800     03 RUB-GROUP                PIC X(60) VALUE 'GROUP'.                 
016200     03 RUB-CATALOG              PIC X(60) VALUE 'CATALOG'.               
016210     03 RUB-HELD-AS              PIC X(60) VALUE 'HELD-AS'.               
016300     03 RUB-CONTAINS             PIC X(60) VALUE 'CONTAINS'.              
016330     SKIP2                                                                
016400 01  CAT-NAMES.                                                           
016500     03                          PIC X(04) VALUE SPACE.                   
016600     03 CAT-NAME-AREA            PIC X(70).                               
016610     SKIP2                                                                
016700 01  CONTAINS-POST.                                                       
016800     03                          PIC X(04) VALUE SPACE.                   
016900     03 CONTAINS-KOMMA           PIC X(1).                                
017000     03 CONTAINS-MBR             PIC X(50).                               
017400*    --- PARAMETRAR TILL POSTSUM                                          
017500*                                                                         
017600*01  -COPY W0005   -PRE  POSTSUM-                                         
017700     EJECT                                                                
017800 01  LSEG-AREA-START             PIC X(24)   VALUE                        
017900                                 'LSEG-AREA-START  '.                     
018000                                                                          
018100 01  IN-LSEG-AREA.                                                        
018200     03 LSEG-STYR                PIC X(01).                               
018300     03 LSEG-NAMN                PIC X(12).                               
018310     03 LSEG-FNAMN1              PIC X(12).                               
018400     03 LSEG-FNAMN2              PIC X(12).                               
021400     EJECT                                                                
021500 01  UTLSEG-AREA-START           PIC X(24)   VALUE                        
021600                                 'UTLSEG-AREA-START  '.                   
021700 01  UT-LSEG-AREA                PIC X(80).                               
021800                                                                          
023400     EJECT                                                                
024500 PROCEDURE DIVISION.                                                      
024600     SKIP2                                                                
024700 STYR SECTION.                                                            
024800                                                                          
024900     PERFORM A-INIT                                                       
025100     PERFORM B-SKAPA-LSEG                                                 
026200                                                                          
026300     PERFORM Z-FINIT                                                      
026400                                                                          
026500     MOVE ZERO TO RETURN-CODE                                             
026600     GOBACK                                                               
026700     .                                                                    
026800     EJECT                                                                
026900 A-INIT SECTION.                                                          
027000                                                                          
027100     OPEN INPUT WDMR74                                                    
027300                                                                          
027400     OPEN OUTPUT WDMR76                                                   
027600     SKIP2                                                                
027700     ACCEPT DAGENS-DATUM  FROM DATE                                       
027800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027900     .                                                                    
028000     EJECT                                                                
028100 B-SKAPA-LSEG SECTION.                                                    
028200                                                                          
028300     PERFORM S01-LAES-WDMR74                                              
028400     PERFORM UNTIL END-OF-WDMR74                                          
028700       IF END-PUNKT = NEJ                                                 
028800         MOVE JA TO END-PUNKT                                             
028900       ELSE                                                               
029000         MOVE '.' TO UT-LSEG-AREA                                         
029100         PERFORM S11-SKRIV-WDMR76                                         
029200       END-IF                                                             
029400       IF LSEG-STYR NOT = 'D'                                             
029500         PERFORM BA-FIRST-NEW-RCD                                         
029510         PERFORM BB-NEXT-NEW-RCD                                          
029600       ELSE                                                               
029700         PERFORM BC-DELETE-RCD                                            
029800       END-IF                                                             
030200                                                                          
030300       PERFORM S01-LAES-WDMR74                                            
030400     END-PERFORM                                                          
030500     MOVE '.' TO UT-LSEG-AREA                                             
030600     PERFORM S11-SKRIV-WDMR76                                             
030700     .                                                                    
030800     EJECT                                                                
030900 BA-FIRST-NEW-RCD SECTION.                                                
030910                                                                          
030920     MOVE LSEG-NAMN TO REPL-MEMBER                                        
031000     MOVE REPLACE-COMMAND TO UT-LSEG-AREA                                 
031100     PERFORM S11-SKRIV-WDMR76                                             
031110                                                                          
031200     MOVE RUB-GROUP       TO UT-LSEG-AREA                                 
031300     PERFORM S11-SKRIV-WDMR76                                             
031310                                                                          
031400     MOVE SPACE           TO UT-LSEG-AREA                                 
031500     PERFORM S11-SKRIV-WDMR76                                             
031600     MOVE RUB-CATALOG     TO UT-LSEG-AREA                                 
031700     PERFORM S11-SKRIV-WDMR76                                             
031701                                                                          
031710     MOVE GROUP-SEGM-CAT TO CAT-NAME-AREA                                 
031720     MOVE CAT-NAMES TO UT-LSEG-AREA                                       
031900     PERFORM S11-SKRIV-WDMR76                                             
031901                                                                          
031910     MOVE SPACE           TO UT-LSEG-AREA                                 
031920     PERFORM S11-SKRIV-WDMR76                                             
032000     MOVE RUB-HELD-AS     TO UT-LSEG-AREA                                 
032100     PERFORM S11-SKRIV-WDMR76                                             
033500     .                                                                    
033600     EJECT                                                                
034900 BB-NEXT-NEW-RCD SECTION.                                                 
034901                                                                          
034903     MOVE SPACE           TO UT-LSEG-AREA                                 
034904     PERFORM S11-SKRIV-WDMR76                                             
034905     MOVE RUB-CONTAINS TO UT-LSEG-AREA                                    
034906     PERFORM S11-SKRIV-WDMR76                                             
034908     MOVE SPACE TO CONTAINS-KOMMA                                         
034910     MOVE LSEG-FNAMN1 TO CONTAINS-MBR                                     
034911     MOVE CONTAINS-POST TO UT-LSEG-AREA                                   
034912     PERFORM S11-SKRIV-WDMR76                                             
034913     IF LSEG-FNAMN2 NOT = SPACE                                           
034914       MOVE ','   TO CONTAINS-KOMMA                                       
034916       MOVE LSEG-FNAMN2 TO CONTAINS-MBR                                   
034917       MOVE CONTAINS-POST TO UT-LSEG-AREA                                 
034918       PERFORM S11-SKRIV-WDMR76                                           
034919     END-IF                                                               
034943     .                                                                    
034944     EJECT                                                                
036716 BC-DELETE-RCD SECTION.                                                   
036720                                                                          
036730     MOVE LSEG-NAMN TO REM-MEMBER                                         
036800     MOVE REMOVE-COMMAND       TO UT-LSEG-AREA                            
036900     PERFORM S11-SKRIV-WDMR76                                             
037000     MOVE NEJ TO END-PUNKT                                                
037200     .                                                                    
037300     EJECT                                                                
070100 Z-FINIT SECTION.                                                         
070200                                                                          
070300     CLOSE WDMR74 WDMR76                                                  
070400     SKIP2                                                                
070500     MOVE 'S' TO POSTSUM-OPKOD                                            
070600     CALL POSTSUM USING POSTSUM-PARM                                      
070700     .                                                                    
070800     EJECT                                                                
070900 S01-LAES-WDMR74  SECTION.                                                
071000     SKIP2                                                                
071100     READ WDMR74 INTO IN-LSEG-AREA                                        
071200     AT END                                                               
071300        SET END-OF-WDMR74 TO TRUE                                         
071400                                                                          
071500     NOT AT END                                                           
071600        MOVE 'WDMR74'    TO POSTSUM-FDNAMN                                
071700        MOVE 'WDMR76D1'  TO POSTSUM-DDNAMN2                               
071800        CALL POSTSUM USING POSTSUM-PARM                                   
071900     END-READ                                                             
072000     .                                                                    
072100     EJECT                                                                
076100 S11-SKRIV-WDMR76 SECTION.                                                
076200     SKIP2                                                                
076300     WRITE LSEG-POST FROM UT-LSEG-AREA                                    
076400                                                                          
076500     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
076600     MOVE 'WDMR76'   TO POSTSUM-FDNAMN                                    
076700     MOVE 'WDMR76D2' TO POSTSUM-DDNAMN2                                   
076800     CALL POSTSUM USING POSTSUM-PARM                                      
076900     .                                                                    
077000     EJECT                                                                
080100 S99-ABEND SECTION.                                                       
080200                                                                          
080300     SKIP2                                                                
080400     MOVE 'S' TO POSTSUM-OPKOD                                            
080500     CALL POSTSUM USING POSTSUM-PARM                                      
080600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
080700     .                                                                    
