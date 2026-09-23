000300     SKIP2                                                                
000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W0090300.                                                 
001000*AUTHOR.        ANDERS WALLIN.                                            
001100*DATE-WRITTEN.  NOV 1981.                                                 
001200                                                                          
001400                                                                          
001500*    FUNKTION:                                                            
001600*        SUBPROGRAM SOM LÄMNAR ETT PRODUKTIONSLÖPNUMMER,                  
001700*        - IDPRODNR - TILL KALLANDE HUVUDPROGRAM.                         
001800                                                                          
001900*        PROGRAMMET HÄMTAR DET SENAST ANVÄNDA LÖPNUMRET FRÅN              
002000*        REGISTRET W00903 OCH RÄKNAR UPP DETSAMMA MED +1 FÖR              
002100*        VARJE CALL FRÅN KALLANDE PROGRAM.                                
002200                                                                          
002300*        REGISTRET OMFATTAR INTERVALLET 90000-99998.                      
002400                                                                          
002500*    ABENDKODER:                                                          
002600*        U0016 - OM FELAKTIG LÄNKAREA FRÅN KALLANDE PROGRAM.              
002700     EJECT                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100*                                                                         
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*--- INFIL:                                                               
003500*                                                                         
003600     SELECT W00903-IN                    ASSIGN TO UT-S-W00903D1.         
003700     SKIP2                                                                
003800*--- UTFIL:                                                               
003900*                                                                         
004000     SELECT W00903-UT                    ASSIGN TO UT-S-W00903D2.         
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W00903-IN                                                            
004700     RECORDING      F                                                     
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
005000*    -COPY W00903       -L.                                               
005200     SKIP3                                                                
005300 FD  W00903-UT                                                            
005400     RECORDING      F                                                     
005500     BLOCK CONTAINS 0.                                                    
005600     SKIP2                                                                
005700*01  POST -COPY W00903      -PRE UT-  -L.                                 
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006001                                                                          
006010*    -- CHECKED BY WY2000                                                 
006100 77  IDPGM                       PIC X(8)    VALUE 'W0090300'.            
006900     SKIP2                                                                
007000 01  GENERELLA-KONSTANTER.                                                
007100                                                                          
007200   03  JA                        PIC X(1)    VALUE 'J'.                   
007300   03  NEJ                       PIC X(1)    VALUE 'N'.                   
007400   03  OPPNA                     PIC X(1)    VALUE 'O'.                   
007500   03  HAMTA-NUMMER              PIC X(1)    VALUE 'N'.                   
007600   03  STANG                     PIC X(1)    VALUE 'S'.                   
007700     SKIP2                                                                
007800 01  END-OF-FILE-SWITCHAR.                                                
007900                                                                          
008000   03  INREG-EOF                 PIC X(1)    VALUE 'N'.                   
008100   03  UTREG-EOF                 PIC X(1)    VALUE 'N'.                   
008200     SKIP3                                                                
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400   03  ABEND                     PIC X(8)   VALUE  'ABEND   '.            
008500     SKIP3                                                                
008600 01  RETURKODER.                                                          
008700   03  RKOD                      PIC S9(4)   VALUE ZERO COMP SYNC.        
008800   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16                    
008900                                                        COMP SYNC.        
009000     EJECT                                                                
009100 01  FILLER                      PIC X(24)   VALUE                        
009200                                       'PRODNUMMER-AREA-START  '.         
009300     SKIP3                                                                
009400*01  PRODNR-AREA    -COPY W00903      -PRE W-                             
009600     EJECT                                                                
009700 LINKAGE SECTION.                                                         
009800*01  AREA -COPY W0090301    -PRE LINK-                                    
010000     EJECT                                                                
010100 PROCEDURE DIVISION USING LINK-AREA.                                      
010200     CONTINUE.                                                            
010300     SKIP2                                                                
010400 STYR SECTION.                                                            
010500     EVALUATE LINK-BEHANDLING                                             
010600     WHEN OPPNA                                                           
010700       PERFORM A-OPPNA-REGISTER                                           
010800     WHEN HAMTA-NUMMER                                                    
010900       PERFORM B-HAMTA-PRODNR                                             
011000     WHEN STANG                                                           
011100       PERFORM C-STANG-REGISTER                                           
011200     WHEN OTHER                                                           
011300       PERFORM S99-ABEND                                                  
011400     END-EVALUATE                                                         
011500     MOVE RKOD TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     CONTINUE.                                                            
011800     EJECT                                                                
011900 A-OPPNA-REGISTER   SECTION.                                              
012000     SKIP2                                                                
012100     OPEN INPUT  W00903-IN                                                
012200          OUTPUT W00903-UT                                                
012300     READ W00903-IN INTO W-PRODNR-AREA                                    
012400     AT END                                                               
012500        MOVE JA TO INREG-EOF                                              
012600     END-READ                                                             
012700     CONTINUE.                                                            
012800     EJECT                                                                
012900 B-HAMTA-PRODNR    SECTION.                                               
013000     SKIP2                                                                
013100     IF W-IDPRODNR < W-IDPRODNR-MAX                                       
013200       ADD +1 TO W-IDPRODNR                                               
013300     ELSE                                                                 
013400       MOVE W-IDPRODNR-MIN TO W-IDPRODNR                                  
013500     END-IF                                                               
013600     MOVE W-IDPRODNR TO LINK-IDPRODNR                                     
013700     CONTINUE.                                                            
013800     EJECT                                                                
013900 C-STANG-REGISTER SECTION.                                                
014000     SKIP2                                                                
014100     WRITE UT-POST FROM W-PRODNR-AREA                                     
014200                                                                          
014300     CLOSE W00903-IN                                                      
014400     CLOSE W00903-UT                                                      
014500     CONTINUE.                                                            
014600     EJECT                                                                
014700 S99-ABEND SECTION.                                                       
014800     SKIP2                                                                
014900     DISPLAY 'FEL I LÄNKAREA TILL W00903'                                 
015000     MOVE RKOD-ABEND-UTAN-DUMP TO RKOD                                    
015100     CALL ABEND USING RKOD                                                
015200     CONTINUE.                                                            
