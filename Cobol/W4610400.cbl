000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4610400.                                                 
001000*AUTHOR.        STIG MULLER.                                              
001100*DATE-WRITTEN.  NOV  1984.                                                
001200                                                                          
001400                                                                          
001500*    FUNKTION:                                                            
001600                                                                          
001700                                                                          
001800*    ABENDKODER:                                                          
001900                                                                          
002000*        U0016    - OM RETURKOD FRÅN SORT                                 
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*- - - - - - - - - - - - INFIL:                                           
002900     SELECT BIP-IN                       ASSIGN TO UT-S-W46104D1.         
003000     SKIP2                                                                
003100*- - - - - - - - - - - - UTFIL:                                           
003200     SELECT W46106                       ASSIGN TO UT-S-W46104D2.         
003300     SKIP2                                                                
003400*- - - - - - - - - - - - SORTFIL:                                         
003500     SELECT SORTFIL                      ASSIGN TO UT-S-W46104DS.         
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  BIP-IN                                                               
004200     RECORDING      F                                                     
004300     BLOCK CONTAINS 0.                                                    
004400     SKIP2                                                                
004500*01  FILLER -COPY W461001    -L.                                          
004600     SKIP2                                                                
004700 FD  W46106                                                               
004800     RECORDING       V                                                    
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP2                                                                
005100*01  UT-POST -COPY W461S001   -L.                                         
005200     EJECT                                                                
005300 SD  SORTFIL                                                              
005400                     .                                                    
005500     SKIP2                                                                
005600*01  POST -COPY W461001    -PRE SORT-.                                    
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005801                                                                          
005810*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM                       PIC X(8)    VALUE 'W4610400'.            
006700     SKIP2                                                                
006800*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
006900                                                                          
007000 77  JA                          PIC X(1)    VALUE 'J'.                   
007100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007400                                                                          
007500 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
007600     EJECT                                                                
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
007900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
008000     SKIP3                                                                
008100*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
008200                                                                          
008300 01  RETURKODER.                                                          
008400   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
008500   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
008600   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
008700     EJECT                                                                
008800*                                                                         
008900*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
009000                                                                          
009100*01  -COPY W0005       -PRE  POSTSUM-.                                    
009200     EJECT                                                                
009300******************************************************************        
009400*         SORTERADE POSTER                                       *        
009500******************************************************************        
009600*01  AREA  -PRE WSORT-  -COPY W461001                                     
009700     EJECT                                                                
009800******************************************************************        
009900*         UTAREA                                                 *        
010000******************************************************************        
010100*01  AREA -COPY W461S001   -PRE UT-.                                      
010200     EJECT                                                                
010300 PROCEDURE DIVISION.                                                      
010400     SKIP2                                                                
010500     PERFORM A-INIT                                                       
010600     SORT SORTFIL ASCENDING                                               
010700     SORT-BIP-IDDISTR                                                     
010800     SORT-BIP-IDKUNDNR                                                    
010900     SORT-BIP-IDORDNR                                                     
011000     SORT-BIP-IDARTNR                                                     
011100     SORT-BIP-IDRONR                                                      
011200     USING   BIP-IN                                                       
011300     OUTPUT PROCEDURE B-BEARBETNING                                       
011400     IF SORT-RETURN > ZERO                                                
011500       DISPLAY '*** W4610400 - FEL VID SORTERING ***'                     
011600       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
011700     ELSE                                                                 
011800       PERFORM Z-FINIT                                                    
011900       MOVE ZERO TO RETURN-CODE                                           
012000       GOBACK                                                             
012100     END-IF                                                               
012200     CONTINUE.                                                            
012300     EJECT                                                                
012400 A-INIT SECTION.                                                          
012500     SKIP2                                                                
012600     OPEN OUTPUT W46106                                                   
012700     CONTINUE.                                                            
012800     EJECT                                                                
012900 B-BEARBETNING SECTION.                                                   
013000     SKIP2                                                                
013100     PERFORM S01-LAS-SORTERAD-BIP-IN                                      
013200     MOVE ZERO TO UT-BIP-SOR0-IDLOPNR                                     
013300     PERFORM UNTIL                                                        
013400      ( SORTFIL-EOF = JA )                                                
013500       MOVE WSORT-BIP-IDDISTR  TO UT-BIP-SOR0-IDDISTR                     
013600       MOVE WSORT-BIP-IDKUNDNR TO UT-BIP-SOR0-IDKUNDNR                    
013700       MOVE WSORT-BIP-IDRONR   TO UT-BIP-SOR0-IDRONR                      
013800       MOVE WSORT-BIP-TIRODAT  TO UT-BIP-SOR0-TIRODAT                     
013900                                                                          
014000       MOVE '001' TO UT-BIP-SOR0-IDPTYP                                   
014100       ADD +1 TO UT-BIP-SOR0-IDLOPNR                                      
014200       MOVE WSORT-BIP-W461001 TO UT-BIP-W461001                           
014300       PERFORM S10-SKRIV-POST                                             
014400       PERFORM S01-LAS-SORTERAD-BIP-IN                                    
014500     END-PERFORM                                                          
014600     CONTINUE.                                                            
014700     EJECT                                                                
014800 S01-LAS-SORTERAD-BIP-IN SECTION.                                         
014900     SKIP2                                                                
015000     RETURN SORTFIL INTO WSORT-AREA                                       
015100                      AT END MOVE JA TO SORTFIL-EOF                       
015200     END-RETURN                                                           
015300                                                                          
015400     IF SORTFIL-EOF = NEJ                                                 
015500                                                                          
015600       MOVE 'BIP-IN'            TO POSTSUM-FDNAMN                         
015700       MOVE 'W46104D1'          TO POSTSUM-DDNAMN2                        
015800       MOVE WSORT-BIP-IDPTYP TO POSTSUM-TRANSTYP                          
015900       CALL POSTSUM   USING POSTSUM-PARM                                  
016000                                                                          
016100     END-IF                                                               
016200     CONTINUE.                                                            
016300     EJECT                                                                
016400 S10-SKRIV-POST SECTION.                                                  
016500     SKIP2                                                                
016600     WRITE UT-POST FROM UT-AREA                                           
016700     MOVE 'W46106'               TO POSTSUM-FDNAMN                        
016800     MOVE 'W46104D2'             TO POSTSUM-DDNAMN2                       
016900     MOVE UT-BIP-SOR0-IDPTYP     TO POSTSUM-TRANSTYP                      
017000     CALL POSTSUM      USING POSTSUM-PARM                                 
017100     CONTINUE.                                                            
017200     EJECT                                                                
017300 Z-FINIT SECTION.                                                         
017400     SKIP2                                                                
017500     CLOSE W46106                                                         
017600     SKIP2                                                                
017700     MOVE 'S' TO POSTSUM-OPKOD                                            
017800     CALL POSTSUM USING POSTSUM-PARM                                      
017900     CONTINUE.                                                            
