000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     W997BS00.                                                
000900*AUTHOR.         ONLINE BS.                                               
001000*DATE-WRITTEN.   MARS    83.                                              
001100*    SKIP2                                                                
001300*    FUNKTION.                                                            
001400*            ANALYS PACKADE KOLLIN                                        
001500                                                                          
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800 INPUT-OUTPUT SECTION.                                                    
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100     SELECT INFIL            ASSIGN UT-S-W997BSD1.                        
002200     SELECT SORTFIL          ASSIGN UT-S-W47310DS.                        
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600     SKIP2                                                                
002700 FD  INFIL                                                                
002800     RECORDING F                                                          
002900     BLOCK 0                                                              
003000                          .                                               
003100     SKIP1                                                                
003200 01  INPOST  PIC X(23).                                                   
003300     SKIP2                                                                
003400 SD  SORTFIL                                                              
003500                     .                                                    
003600     SKIP1                                                                
003700 01  SORT-POST.                                                           
003800     03  SORT-IDPRODNR         PIC S9(7)  COMP-3.                         
003900     03  SORT-IDDISTR          PIC S9(5)  COMP-3.                         
004000     03  SORT-KDCLAGER         PIC S9(1)  COMP-3.                         
004100     03  SORT-KDORDSTA         PIC S9(1)  COMP-3.                         
004200     03  SORT-KVKOLLI          PIC S9(5)  COMP-3.                         
004300     03  SORT-IDKOLLI          PIC S9(5)  COMP-3.                         
004400     03  SORT-KDKOLSTA         PIC S9(1)  COMP-3.                         
004500     03  SORT-KVORDRAD         PIC S9(5)  COMP-3.                         
004600     03  SORT-TIPACKN          PIC S9(7)  COMP-3.                         
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004801                                                                          
004810*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                   PIC X(8)    VALUE 'W997BS00'.                
005400     SKIP2                                                                
005500 01  RKOD                    PIC S9(4)   VALUE ZERO  COMP SYNC.           
005600     SKIP2                                                                
005700 01  KONSTANTER.                                                          
005800     03  JA                  PIC X       VALUE 'J'.                       
005900     03  NEJ                 PIC X       VALUE 'N'.                       
006000     03  EOF-INFIL           PIC X       VALUE 'N'.                       
006100     03  EOF-SORT            PIC X       VALUE 'N'.                       
006200     SKIP2                                                                
006300     SKIP2                                                                
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500     03  ABEND               PIC X(8)    VALUE 'ABEND   '.                
006600     EJECT                                                                
006700 01  FILLER    PIC X(32)   VALUE 'INPOST AREA'.                           
006800     SKIP2                                                                
006900 01  KOLI-POST.                                                           
007000     03  KOLI-IDPRODNR         PIC S9(7)  COMP-3.                         
007100     03  KOLI-IDDISTR          PIC S9(5)  COMP-3.                         
007200     03  KOLI-KDCLAGER         PIC S9(1)  COMP-3.                         
007300     03  KOLI-KDORDSTA         PIC S9(1)  COMP-3.                         
007400     03  KOLI-KVKOLLI          PIC S9(5)  COMP-3.                         
007500     03  KOLI-IDKOLLI          PIC S9(5)  COMP-3.                         
007600     03  KOLI-KDKOLSTA         PIC S9(1)  COMP-3.                         
007700     03  KOLI-KVORDRAD         PIC S9(5)  COMP-3.                         
007800     03  KOLI-TIPACKN          PIC S9(7)  COMP-3.                         
007900     SKIP3                                                                
008000 01  MAX-C1                    PIC S9(5)  COMP-3.                         
008100 01  MAX-C2                    PIC S9(5)  COMP-3.                         
008200 01  IX                        PIC S9(3)  COMP-3.                         
008300 01  IY                        PIC S9(3)  COMP-3.                         
008400     SKIP3                                                                
008500 01  TAB-C1.                                                              
008600     03  DAG-C1 OCCURS 5.                                                 
008700         05  VARDE-C1  OCCURS 11 PIC S9(5) COMP-3.                        
008800     SKIP3                                                                
008900 01  TAB-C2.                                                              
009000     03  DAG-C2 OCCURS 5.                                                 
009100         05  VARDE-C2  OCCURS 11 PIC S9(5) COMP-3.                        
009200     EJECT                                                                
009300 PROCEDURE DIVISION.                                                      
009400     SKIP1                                                                
009500     PERFORM A-INITIERA                                                   
009600     SKIP1                                                                
009700     SORT SORTFIL                                                         
009800          ASCENDING KEY                                                   
009900          SORT-KDCLAGER                                                   
010000          SORT-TIPACKN                                                    
010100     SKIP1                                                                
010200     USING INFIL                                                          
010300     OUTPUT PROCEDURE HUVUT                                               
010400     SKIP1                                                                
010500     IF  SORT-RETURN > +0                                                 
010600       DISPLAY 'SORTERINGSFEL'                                            
010700       MOVE +1200 TO RKOD                                                 
010800       CALL ABEND USING RKOD                                              
010900     END-IF                                                               
011000     SKIP1                                                                
011100     PERFORM Z-AVSLUTA                                                    
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     CONTINUE.                                                            
011500     EJECT                                                                
011600 A-INITIERA SECTION.                                                      
011700     SKIP1                                                                
011800     OPEN INPUT INFIL                                                     
011900     SKIP1                                                                
012000     MOVE ZERO TO MAX-C1 MAX-C2                                           
012100     SKIP1                                                                
012200     MOVE +1   TO IX IY                                                   
012300     PERFORM UNTIL                                                        
012400      NOT ( IX < 6 )                                                      
012500       PERFORM UNTIL                                                      
012600        NOT ( IY < 12 )                                                   
012700         MOVE ZERO TO VARDE-C1 (IX, IY)                                   
012800         MOVE ZERO TO VARDE-C2 (IX, IY)                                   
012900         ADD +1 TO IY                                                     
013000       END-PERFORM                                                        
013100       ADD +1 TO IX                                                       
013200       MOVE +1 TO IX                                                      
013300     END-PERFORM                                                          
013400     CONTINUE.                                                            
013500     EJECT                                                                
013600 HUVUT SECTION.                                                           
013700     SKIP1                                                                
013800     PERFORM S11-LAS-SORT                                                 
013900     PERFORM UNTIL                                                        
014000      NOT ( EOF-SORT = NEJ )                                              
014100       PERFORM S11-LAS-SORT                                               
014200                                                                          
014300     END-PERFORM                                                          
014400     CONTINUE.                                                            
014500     EJECT                                                                
014600 Z-AVSLUTA SECTION.                                                       
014700     SKIP1                                                                
014800     CLOSE INFIL                                                          
014900     CONTINUE.                                                            
015000     SKIP1                                                                
015100     EJECT                                                                
015200 S11-LAS-SORT SECTION.                                                    
015300     SKIP1                                                                
015400     RETURN SORTFIL                                                       
015500     AT END                                                               
015600     MOVE JA TO EOF-SORT                                                  
015700     END-RETURN                                                           
015800     CONTINUE.                                                            
