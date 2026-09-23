000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4406400.                                    
000300 AUTHOR.                     STEFANO GIOBBI.                              
000400     DATE-WRITTEN.           JUN 1991.                                    
000500*                                                                         
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
001000*    LÄSER VORKÖN (WDR4) MED SB.                                          
001100*    SALDOINFORMATION LISTAS PÅ FIL.                                      
001200*                                                                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800                                                                          
001900     SELECT W44064           ASSIGN TO      W44064D1.                     
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 FILE SECTION.                                                            
002300     SKIP2                                                                
002400 FD  W44064                                                               
002500     LABEL RECORD STANDARD                                                
002600     RECORDING F                                                          
002700     BLOCK CONTAINS 0.                                                    
002800*01  POST -COPY W440064   -PRE W44064-  -L.                               
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100     SKIP2                                                                
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400*    ---- GENERELLA KONSTANTER                                            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 01  DYNAMISKA-SUBPROGRAM.                                                
003900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
004000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
004100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
004200     EJECT                                                                
004300*    ---- PARAMETRAR TILL POSTSUM                                         
004400                                                                          
004500*01  -COPY W0005      -PRE POSTSUM-.                                      
004600     EJECT                                                                
004700*    ---- UTAREA FÖR W44064-POST                                          
004800                                                                          
004900 01  FILLER                      PIC X(16)   VALUE                        
005000                                             'W-W44064-POST'.             
005100     SKIP3                                                                
005200*01  AREA -COPY W440064    -PRE UT-.                                      
005300     EJECT                                                                
005400                                                                          
005500*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
005600                                                                          
005700 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
005800                                                                          
005900*    ---- STATUSKOD FRÅN IMS                                              
006000                                                                          
006100 01  STATUS-WS                   PIC XX.                                  
006200     88  SEGMENT-FINNS                      VALUE '  '.                   
006300     88  SEGMENT-SLUT                       VALUE 'GB'.                   
006400                                                                          
006500 01  GODK-STATUSKODER.                                                    
006600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
006700                                                                          
006800 01  SSA1                        PIC X(40).                               
006900 01  SSA2                        PIC X(40).                               
007000                                                                          
007100     EJECT                                                                
007200*01      -COPY W0003.                                                     
007300     EJECT                                                                
007400 01  FILLER                      PIC X(16)  VALUE                         
007500                                            'DLI-IO-AREA'.                
007600 01  DLI-IO-AREA.                                                         
007700*                                                                         
007800*  03   -COPY WDGX4542                                                    
007900     EJECT                                                                
008000 LINKAGE SECTION.                                                         
008100     SKIP2                                                                
008200*    -COPY W0008 -PRE WDR4-.                                              
008300    05  FILLER                   PIC XX.                                  
008400     EJECT                                                                
008500 PROCEDURE DIVISION  USING WDR4-PCB.                                      
008600     ENTRY 'DLITCBL' USING WDR4-PCB.                                      
008700                                                                          
008800 STYR SECTION.                                                            
008900                                                                          
009000     PERFORM A-INIT                                                       
009100     PERFORM IMS-GET-WDR4                                                 
009200     PERFORM UNTIL SEGMENT-SLUT                                           
009300       PERFORM B-SKRIV-UTPOST                                             
009400       PERFORM IMS-GET-WDR4                                               
009500     END-PERFORM                                                          
009600                                                                          
009700     PERFORM Z-FINIT                                                      
009800     MOVE    ZERO TO RETURN-CODE                                          
009900     GOBACK                                                               
010000     .                                                                    
010100     EJECT                                                                
010200 A-INIT SECTION.                                                          
010300                                                                          
010400     OPEN OUTPUT W44064                                                   
010500     MOVE 'W4406400'         TO POSTSUM-PROGNAMN                          
010600     MOVE 'W44064D1'         TO POSTSUM-DDNAMN2                           
010700     MOVE 'W44064  '         TO POSTSUM-FDNAMN                            
010800     .                                                                    
010900     EJECT                                                                
011000 B-SKRIV-UTPOST SECTION.                                                  
011100                                                                          
011200     IF (4542-KVBEART-Q NOT = 4542-KVPREAVB)                              
011300            AND (4542-KDVORATG = '0' OR '1')                              
011400                                                                          
011500       MOVE    4542-IDARTNR  TO    UT-IDARTNR                             
011600                                                                          
011700       COMPUTE UT-KVPRERO =   4542-KVBEART-Q                              
011800                            - 4542-KVPREAVB                               
011900                                                                          
012000       WRITE   W44064-POST   FROM  UT-AREA                                
012100                                                                          
012200       MOVE   'VOR '         TO    POSTSUM-TRANSTYP                       
012300       CALL    POSTSUM       USING POSTSUM-PARM                           
012400                                                                          
012500     END-IF                                                               
012600     .                                                                    
012700     EJECT                                                                
012800 Z-FINIT SECTION.                                                         
012900                                                                          
013000     CLOSE W44064                                                         
013100     MOVE  'S'     TO    POSTSUM-OPKOD                                    
013200     CALL  POSTSUM USING POSTSUM-PARM                                     
013300     .                                                                    
013400     EJECT                                                                
013500*                                                                         
013600*                                                                         
013700*                                                                         
013800*                  IIIIIIIII  MMMMMMMMM  SSSSSSSSS                        
013900*                  II     II  M MMMMM M  SSS   SSS                        
014000*                  IIII IIII  M  MMM  M  SS SSS SS                        
014100*                  IIII IIII  M M M M M  SS  SSSSS                        
014200*                  IIII IIII  M MM MM M  SSSSS  SS                        
014300*                  IIII IIII  M MMMMM M  SS SSS SS                        
014400*                  II     II  M MMMMM M  SSS   SSS                        
014500*                  IIIIIIIII  MMMMMMMMM  SSSSSSSSS                        
014600*                                                                         
014700*                                                                         
014800*                                                                         
014900 IMS-GET-WDR4 SECTION.                                                    
015000                                                                          
015100     MOVE    'WL454101(IDHTYP   =4541)' TO    SSA1                        
015200     MOVE    'WL454111'                 TO    SSA2                        
015300     MOVE    '  GEGB'                   TO    GODK-STATUSKODER            
015400     CALL    CBLTDLI          USING GN   WDR4-PCB DLI-IO-AREA             
015500                                    SSA1 SSA2                             
015600     MOVE    WDR4-STATUS-CODE TO    STATUS-WS                             
015700     PERFORM IMS-STATUSKONTROLL                                           
015800     .                                                                    
015900     SKIP3                                                                
016000 IMS-STATUSKONTROLL SECTION.                                              
016100                                                                          
016200     SET    STATUS-IX TO 1                                                
016300     SEARCH GODK-STATUS                                                   
016400       AT END CALL FELLOG                                                 
016500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016600         CONTINUE                                                         
016700     END-SEARCH                                                           
016800     .                                                                    
