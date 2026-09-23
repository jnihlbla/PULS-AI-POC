000300     SKIP2                                                                
000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W0301400.                                                 
001000*AUTHOR.        KJELL                                                     
001100*DATE-WRITTEN.  JAN 1987                                                  
001300                                                                          
001400*    FUNKTION:                                                            
001500                                                                          
001600*        SKAPAR ETT DATUMKORT INNEHÅLLANDE SAMMA                          
001700*        UPPGIFTER SOM VOLVODATAS STANDARDDATUMKORT                       
001800*        (ID 000000)                                                      
001900                                                                          
002000                                                                          
002100*    SUBPROGRAM 1:                                                        
002200*        WDATKONV - DAT-WDATAREA, PROGRAMMET KONVERTERAR DATUM            
002300*                 TILL OLIKA FORMAT.                                      
002400                                                                          
002500*    ABENDKODER:                                                          
002600*        U0016 - VID FELAKTIG ANGIVNING AV DATUM.                         
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*--- UTFILER:                                                             
003500                                                                          
003600     SELECT UT-DATUMKORT             ASSIGN TO  W03014D2.                 
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP2                                                                
004200 FD  UT-DATUMKORT                                                         
004300     RECORDING      F                                                     
004400     BLOCK CONTAINS 0.                                                    
004500     SKIP2                                                                
004600 01  DATUMKORT.                                                           
004700     03 DATUM                    PIC X(5).                                
004800     03 FAELT2                   PIC X(6).                                
004900     03 D-AAR                    PIC 99.                                  
005000     03 D-MAANAD                 PIC 99.                                  
005100     03 D-DAG                    PIC 99.                                  
005200     03 D-VECKA                  PIC 99.                                  
005300     03 D-DAGNR                  PIC 9.                                   
005400     03 D-PERIOD                 PIC 9.                                   
005500     03 D-64DEL                  PIC 99.                                  
005600     03 FILLER                   PIC X(52).                               
005700     03 D-IOCSDAT                PIC 9(5).                                
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
005901                                                                          
005910*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(8)    VALUE 'W0301400'.            
006600 01  RETURKODER.                                                          
006700*                                                                         
006800     03  RKOD                    PIC S9(4)   COMP SYNC VALUE ZERO.        
006900     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   COMP SYNC VALUE +16.         
007000                                                                          
007100                                                                          
007200 01  DAGENS-DATUM                PIC X(6).                                
007300                                                                          
007400                                                                          
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007900     EJECT                                                                
008000*    -COPY WDATAREAC0                                                     
008200     EJECT                                                                
008300 LINKAGE SECTION.                                                         
008400                                                                          
008500 01  EXEC-PARM.                                                           
008600     03 LAENGD                  PIC S9(4) COMP.                           
008700     03 PARM-DATUM              PIC X(6).                                 
008800     EJECT                                                                
008900 PROCEDURE DIVISION USING EXEC-PARM.                                      
009000     CONTINUE.                                                            
009100     SKIP2                                                                
009200 STYR SECTION.                                                            
009300                                                                          
009400     PERFORM A-INIT                                                       
009500     PERFORM B-CHECK-PARM-DATUM                                           
009600     PERFORM C-FLYTTA-BERAKNA-UTFIL                                       
009700     PERFORM Z-FINIT                                                      
009800     MOVE RKOD TO RETURN-CODE                                             
009900     GOBACK                                                               
010000     CONTINUE.                                                            
010100     EJECT                                                                
010200 A-INIT SECTION.                                                          
010300                                                                          
010400     OPEN OUTPUT UT-DATUMKORT                                             
010500     MOVE SPACE                  TO DATUMKORT                             
010600     CONTINUE.                                                            
010700     EJECT                                                                
010800                                                                          
010900 B-CHECK-PARM-DATUM SECTION.                                              
011000     IF LAENGD = 0                                                        
011100       ACCEPT DAGENS-DATUM FROM DATE                                      
011200     ELSE                                                                 
011300       EVALUATE TRUE                                                      
011400       WHEN LAENGD = 6                                                    
011500         IF PARM-DATUM IS NUMERIC                                         
011600           MOVE PARM-DATUM       TO DAGENS-DATUM                          
011700         ELSE                                                             
011800           DISPLAY 'DATUM ANGIVEN I JCL:EN ÄR EJ NUMERISK.'               
011900           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
012000         END-IF                                                           
012100        WHEN OTHER                                                        
012200         DISPLAY 'DATUM ANGIVEN I JCL:EN ÄR INTE 6 POS LÅNG.'             
012300         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
012400       END-EVALUATE                                                       
012500     END-IF                                                               
012600     CONTINUE.                                                            
012700     EJECT                                                                
012800                                                                          
012900 C-FLYTTA-BERAKNA-UTFIL SECTION.                                          
013000                                                                          
013100     MOVE 'DATE1'              TO DATUM                                   
013200     MOVE '000000'             TO FAELT2                                  
013300     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
013400     MOVE DAGENS-DATUM         TO DAT-I-TIDATUM                           
013500                                                                          
013600     CALL WDATKONV USING DAT-KDDATFORM                                    
013700                         DAT-I-TIDATUM                                    
013800                         DAT-O-TIDATUM                                    
013900                         DAT-KDSVAR                                       
014000                                                                          
014100     MOVE DAT-TIAA             TO D-AAR                                   
014200     MOVE DAT-TIMM             TO D-MAANAD                                
014300     MOVE DAT-TIDD             TO D-DAG                                   
014400     MOVE DAT-TIVV             TO D-VECKA                                 
014500     MOVE DAT-TID              TO D-DAGNR                                 
014600     MOVE DAT-TIP              TO D-PERIOD                                
014700     MOVE ZERO                 TO D-64DEL                                 
014800     MOVE DAT-TIAADDD          TO D-IOCSDAT                               
014900     CONTINUE.                                                            
015000     EJECT                                                                
015100     EJECT                                                                
015200 Z-FINIT SECTION.                                                         
015300                                                                          
015400     WRITE DATUMKORT                                                      
015500     CLOSE UT-DATUMKORT                                                   
015600     CONTINUE.                                                            
