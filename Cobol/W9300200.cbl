000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9300200.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   02/02/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER VALUTAINFO IFRÅN BILLIT                                    
000900*        O SKAPAR EN FIL FÖR MÅNADSUPPDATERING AV KURSER                  
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W93002T                                             
001300*        REQUEST:     W93002I1                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        RESPONSE:    W93002O1                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- UTFIL1 OMRÄKNADE VALUTOR TILL WDG2 BASEN                   
002600     SELECT W93001                     ASSIGN TO W93002D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W93001                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY W93001   -PRE  UT-   -L.                                  
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(08)   VALUE 'W9300200'.            
004100                                                                          
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100                                                                          
005200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005300 01  GENERELLA-SUBPROGRAM.                                                
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005600     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
005700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005800     SKIP3                                                                
005900*    --- PARAMETRAR TILL ABEND                                            
006000                                                                          
006100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006400     SKIP3                                                                
006500 01  MESSAGE-CODES.                                                       
006600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL POSTSUM                                          
006900*                                                                         
007000*01  -COPY W0005   -PRE  POSTSUM-                                         
007100     EJECT                                                                
007200 01  UT-AREA-START               PIC X(24)   VALUE  'UT-AREA'.            
007300                                                                          
007400*01  AREA -COPY W93001     -PRE UT-                                       
007500     EJECT                                                                
007600                                                                          
007700 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
007800     SKIP3                                                                
007900 01  -COPY WZ01RECV                                                       
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
008200     SKIP3                                                                
008300 01  RECV-AREA.                                                           
008400*    03  -COPY WF10P002     -PRE BIL-                                     
008500     EJECT                                                                
008600                                                                          
008700 LINKAGE SECTION.                                                         
008800*01  -COPY W0009   -PRE MSG-                                              
008900     EJECT                                                                
009000                                                                          
009100 PROCEDURE DIVISION USING MSG-PCB.                                        
009200 MAIN SECTION.                                                            
009300     ENTRY 'DLITCBL' USING MSG-PCB.                                       
009400                                                                          
009500     PERFORM S03-LAES-OPEN                                                
009600     PERFORM S03-LAES-MEDDELANDE                                          
009700     IF RECV-KDRC = 0                                                     
009800       PERFORM A-INIT                                                     
009900       IF NYCKLAR-OK                                                      
010000         PERFORM F-RECIVE                                                 
010100       END-IF                                                             
010200     END-IF                                                               
010300                                                                          
010400     PERFORM S03-LAES-CLOSE                                               
010500                                                                          
010600     PERFORM Z-FINIT                                                      
010700     MOVE ZERO TO RETURN-CODE                                             
010800     GOBACK                                                               
010900     .                                                                    
011000     EJECT                                                                
011100 A-INIT SECTION.                                                          
011200                                                                          
011300     OPEN OUTPUT W93001                                                   
011400                                                                          
011500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011600     .                                                                    
011700     EJECT                                                                
011800 F-RECIVE SECTION.                                                        
011900                                                                          
012000     PERFORM UNTIL RECV-KDRC > 0                                          
012100       IF BIL-IDLEGSEL = 'VCCS'                                           
012200         PERFORM FA-KONTROLLERA-O-SKRIV                                   
012300       END-IF                                                             
012400       PERFORM S03-LAES-MEDDELANDE                                        
012500     END-PERFORM                                                          
012600     .                                                                    
012700     EJECT                                                                
012800 FA-KONTROLLERA-O-SKRIV SECTION.                                          
012900                                                                          
013000     MOVE 05                    TO UT-SAP-IDFTG                           
013100     MOVE BIL-KDVALISO          TO UT-SAP-KDVALISO                        
013200     MOVE SPACE                 TO UT-SAP-FILLERX                         
013300     IF  BIL-REVALUTA-FROM > 9999                                         
013400         COMPUTE UT-SAP-REVALUTA =                                        
013500                 BIL-REVALUTA-FROM / 100                                  
013600         END-COMPUTE                                                      
013700         COMPUTE UT-SAP-PRKURS   =                                        
013800                 BIL-PRKURS   / 100                                       
013900         END-COMPUTE                                                      
014000     ELSE                                                                 
014100       IF BIL-REVALUTA-FROM > 999                                         
014200           COMPUTE UT-SAP-REVALUTA =                                      
014300                   BIL-REVALUTA-FROM / 10                                 
014400           END-COMPUTE                                                    
014500           COMPUTE UT-SAP-PRKURS =                                        
014600                   BIL-PRKURS / 10                                        
014700           END-COMPUTE                                                    
014800       ELSE                                                               
014900           MOVE BIL-REVALUTA-FROM TO UT-SAP-REVALUTA                      
015000           MOVE BIL-PRKURS       TO UT-SAP-PRKURS                         
015100       END-IF                                                             
015200     END-IF                                                               
015300     MOVE BIL-DASTADAT    TO UT-SAP-TISTADAT                              
015400     MOVE ZERO            TO UT-SAP-TISTODAT                              
015500     PERFORM S11-SKRIV-W93001                                             
015600     .                                                                    
015700     EJECT                                                                
015800 Z-FINIT SECTION.                                                         
015900                                                                          
016000                                                                          
016100     CLOSE W93001                                                         
016200                                                                          
016300     MOVE 'S' TO POSTSUM-OPKOD                                            
016400     CALL POSTSUM USING POSTSUM-PARM                                      
016500     .                                                                    
016600     EJECT                                                                
016700*    --- DISPATCHER-SEKTIONER                                             
016800 S03-LAES-OPEN SECTION.                                                   
016900                                                                          
017000     MOVE 'OPEN'                     TO RECV-KDFUNC                       
017100     MOVE 'CARPARTS.PULS.RECCURRENCY' TO RECV-ADDISPABS                   
017200     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-OPEN-AREA                 
017300                                                                          
017400     IF RECV-KDRC > 0                                                     
017500       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
017600       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
017700       DELIMITED BY SIZE INTO FELTEXT                                     
017800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
017900     END-IF                                                               
018000     .                                                                    
018100     SKIP3                                                                
018200 S03-LAES-MEDDELANDE SECTION.                                             
018300                                                                          
018400     MOVE 'GET'                      TO RECV-KDFUNC                       
018500     MOVE LENGTH OF RECV-AREA        TO RECV-KVDLEN                       
018600     CALL WZ01RECV USING RECV-CONTROL-AREA RECV-KVDLEN RECV-AREA          
018700                                                                          
018800     IF RECV-KDRC > 1                                                     
018900       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
019000       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
019100       DELIMITED BY SIZE INTO FELTEXT                                     
019200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
019300     END-IF                                                               
019400     .                                                                    
019500     SKIP3                                                                
019600 S03-LAES-CLOSE SECTION.                                                  
019700                                                                          
019800     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
019900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
020000                                                                          
020100     IF RECV-KDRC > 0                                                     
020200       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
020300       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
020400       DELIMITED BY SIZE INTO FELTEXT                                     
020500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
020600     END-IF                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 S11-SKRIV-W93001 SECTION.                                                
021000                                                                          
021100     WRITE UT-POST FROM UT-AREA                                           
021200                                                                          
021300     MOVE 'UT1'      TO POSTSUM-TRANSTYP                                  
021400     MOVE 'W93001 '  TO POSTSUM-FDNAMN                                    
021500     MOVE 'W93002D1' TO POSTSUM-DDNAMN2                                   
021600     CALL POSTSUM USING POSTSUM-PARM                                      
021700     .                                                                    
