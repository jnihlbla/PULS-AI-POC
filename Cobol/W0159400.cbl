001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W0159400.                                                
001300*AUTHOR.         THOMAS NILSSON.                                          
001400*DATE-WRITTEN.   92/07/29.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        KOLLAR OM IDORDER PÅ WDE9 FINNS PÅ WDE8                          
002000*                                                                         
002110*        PROGRAMMET LÄSER      WDE9 MED SB                                
002700                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401                                                                          
003402*          --- SAKNAS PÅ WDE8                                             
003410     SELECT W01594                     ASSIGN TO W01594D1.                
003420     SELECT W01595                     ASSIGN TO W01594D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800                                                                          
003900 FILE SECTION.                                                            
004001                                                                          
004002 FD  W01594                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005     SKIP2                                                                
004010*01  POST -COPY WDE901 -PRE  UT-  -L.                                     
004020                                                                          
004030 FD  W01595                                                               
004040     RECORDING       F                                                    
004050     BLOCK CONTAINS  0.                                                   
004060     SKIP2                                                                
004070 01  INPOST PIC X(80).                                                    
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W0159400'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  EOF                         PIC X       VALUE 'N'.                   
004900                                                                          
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200                                                                          
006300 01  TABELL.                                                              
006310   03  INTABELL OCCURS 335 INDEXED BY IX.                                 
006400     05  IDORDER                 PIC S9(7)   COMP-3.                      
006500                                                                          
006600 01  INAREA.                                                              
006700     03  INIDORDER               PIC S9(7)   COMP-3.                      
006800     03  FILLER                  PIC X(76).                               
007301     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900                                                                          
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-IDORDER-X.                                                     
008110         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008610     88  BASEN-SLUT                          VALUE 'GB'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(32).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010000                                                                          
010100 01  DLI-IO-AREA.                                                         
010200     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
010301                                                                          
010302     03  E9 REDEFINES IO-AREA.                                            
010310*        05  -COPY WDE901                                                 
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010800                                                                          
010902*01  -COPY W0008  -PRE E9-                                                
010910     05  FILLER                  PIC X.                                   
011101 PROCEDURE DIVISION  USING E9-PCB.                                        
011110     ENTRY 'DLITCBL' USING E9-PCB.                                        
011200                                                                          
011500     OPEN OUTPUT W01594                                                   
011600          INPUT  W01595                                                   
011610     SET IX TO 1                                                          
011611     PERFORM UNTIL EOF = JA                                               
011620       READ W01595 INTO INAREA                                            
011621       AT END MOVE JA TO EOF                                              
011622       NOT AT END MOVE INIDORDER TO IDORDER(IX)                           
011623       END-READ                                                           
011630       SET IX UP BY 1                                                     
011640     END-PERFORM                                                          
011700     PERFORM IMS-GN-E9                                                    
011710     PERFORM UNTIL BASEN-SLUT                                             
011720       SET IX TO 1                                                        
011730       SEARCH INTABELL                                                    
011740       AT END                                                             
011750           WRITE UT-POST FROM IO-AREA                                     
011760       WHEN PRAD-IDORDER = IDORDER(IX)                                    
011770         CONTINUE                                                         
011780       END-SEARCH                                                         
012110       PERFORM IMS-GN-E9                                                  
012500     END-PERFORM                                                          
012710     CLOSE W01594 W01595                                                  
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
015902 IMS-GN-E9   SECTION.                                                     
015903                                                                          
015904     CALL CBLTDLI USING GN E9-PCB DLI-IO-AREA                             
015905     MOVE E9-STATUS-CODE TO STATUS-WS                                     
015906     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015907     PERFORM IMS-STATUSKONTROLL                                           
015910     .                                                                    
016000                                                                          
016100 IMS-STATUSKONTROLL SECTION.                                              
016200     SKIP2                                                                
016300     SET STATUS-IX TO 1                                                   
016400     SEARCH GODK-STATUS                                                   
016500       AT END                                                             
016800         CALL FELLOG                                                      
016900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
