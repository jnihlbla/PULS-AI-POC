000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4121100.                                                
000400*AUTHOR.         LASSI OLGRENER.                                          
000500*DATE-WRITTEN.   93/10/27.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER SISTA VECKANS Q4-RADER MED KLASS 0 TILL EN FIL         
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDQ4 MED SB                                
001300                                                                          
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*          --- UTFIL VECKANS KLASS-0:OR                                   
002100     SELECT W41211                     ASSIGN TO W41211D1.                
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400                                                                          
002500 FILE SECTION.                                                            
002600                                                                          
002700 FD  W41211                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000     SKIP2                                                                
003100 01  UTPOST -COPY W41211  -L.                                             
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP2                                                                
003401                                                                          
003410*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)    VALUE 'W4121100'.            
003510 77  JA                          PIC X(8)    VALUE 'J'.                   
003520 77  NEJ                         PIC X(8)    VALUE 'N'.                   
003530 77  FELTEXT                     PIC X(32)   VALUE SPACE.                 
003540 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
003610 77  W-AKT-VECKA                 PIC 9(4)    VALUE ZERO.                  
003700                                                                          
003710 01  VECKA-SW                    PIC X.                                   
003720     88  VECKA-OK                            VALUE 'J'.                   
003730                                                                          
003800 01  DYNAMISKA-SUBPROGRAM.                                                
003900*                                                                         
004000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004101     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004110     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
004300     EJECT                                                                
004400*    --- PARAMETRAR TILL WDATKONV                                         
004500*01  -COPY WDATAREA                                                       
004600     EJECT                                                                
004610*    --- PARAMETRAR TILL POSTSUM                                          
004620*01  -COPY W0005   -PRE POSTSUM-                                          
004630     EJECT                                                                
004700 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
004800 01  AREA -COPY W41211    -PRE UT-                                        
007100     EJECT                                                                
007200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007300*                                                                         
007400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007500                                                                          
007600*    --- STATUS-KOD FRÅN IMS                                              
007700 01  STATUS-WS                   PIC XX.                                  
007800     88  SEGMENT-FINNS                       VALUE '  '.                  
007900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008000     88  BASEN-SLUT                          VALUE 'GB'.                  
008100     SKIP2                                                                
008200 01  GODK-STATUSKODER.                                                    
008300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008400     EJECT                                                                
008500*    --- IMS FUNKTIONSKODER                                               
008600*01  -COPY W0003                                                          
008700     EJECT                                                                
008800*    ---  DLI INPUT-OUTPUT AREA                                           
008900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009000                                                                          
009100 01  DLI-IO-AREA.                                                         
009200                                                                          
009500   03  -COPY WDQ401                                                       
009600     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500 01  -COPY W0008  -PRE WDQ4-                                              
010600     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010800 PROCEDURE DIVISION  USING WDQ4-PCB.                                      
010900*    ENTRY 'DLITCBL' USING WDQ4-PCB.                                      
011000                                                                          
011010     PERFORM A-INIT                                                       
011400                                                                          
011500     PERFORM IMS-GN-WDQ4                                                  
011600     PERFORM UNTIL BASEN-SLUT OR SEGMENT-SAKNAS                           
011901       IF ORAD-KDORDKL = +0                                               
011910         PERFORM B-KOLLA-REGDAT                                           
011911         IF VECKA-OK                                                      
011912           PERFORM S01-SKRIV-W41211                                       
011920         END-IF                                                           
011940       END-IF                                                             
011950       PERFORM IMS-GN-WDQ4                                                
012030     END-PERFORM                                                          
012070                                                                          
012080     CLOSE W41211                                                         
012090     MOVE 'S' TO POSTSUM-OPKOD                                            
012100     CALL POSTSUM USING POSTSUM-PARM                                      
012200     MOVE ZERO TO RETURN-CODE                                             
012300     GOBACK                                                               
012400     .                                                                    
012500     EJECT                                                                
012510 A-INIT SECTION.                                                          
012511                                                                          
012520     OPEN OUTPUT W41211                                                   
012530     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
012531                                                                          
012532     MOVE 'IDAG'          TO DAT-KDDATFORM                                
012534                                                                          
012535     CALL WDATKONV  USING DAT-KDDATFORM                                   
012536                          DAT-I-TIDATUM                                   
012537                          DAT-O-TIDATUM                                   
012538                          DAT-KDSVAR                                      
012539     IF DAT-KDSVAR-FEL                                                    
012540        MOVE 'FEL FRÅN WDATKONV I A-SECTION'                              
012541                          TO FELTEXT                                      
012542        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
012543     END-IF                                                               
012544     MOVE DAT-TIAAVV-GRP  TO W-AKT-VECKA                                  
012545     MOVE W-AKT-VECKA     TO UT-TIAAVV                                    
012546     .                                                                    
012550     EJECT                                                                
012600 B-KOLLA-REGDAT SECTION.                                                  
012700                                                                          
012810     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
012820     MOVE ORAD-TIREGDAT   TO DAT-I-TIDATUM                                
012900     CALL WDATKONV  USING DAT-KDDATFORM                                   
013000                          DAT-I-TIDATUM                                   
013100                          DAT-O-TIDATUM                                   
013200                          DAT-KDSVAR                                      
013300     IF DAT-KDSVAR-FEL                                                    
013301        MOVE 'FEL FRÅN WDATKONV I B-SECTION'                              
013302                          TO FELTEXT                                      
013306        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
013310     END-IF                                                               
013368     IF DAT-TIAAVV-GRP = W-AKT-VECKA                                      
013369       MOVE JA            TO VECKA-SW                                     
013370       MOVE ORAD-IDDISTR  TO UT-IDDISTR                                   
013371       MOVE ORAD-IDARTNR  TO UT-IDARTNR                                   
013372       MOVE ORAD-KVBEART  TO UT-KVBEART                                   
013373       MOVE SPACE         TO UT-KDVORATG                                  
013374     ELSE                                                                 
013375       MOVE NEJ           TO VECKA-SW                                     
013376     END-IF                                                               
013377     .                                                                    
013378     EJECT                                                                
013460 S01-SKRIV-W41211 SECTION.                                                
013470                                                                          
013480     WRITE UTPOST FROM UT-AREA                                            
013490                                                                          
013500     MOVE 'W41211'   TO POSTSUM-FDNAMN                                    
013600     MOVE 'W41211D1' TO POSTSUM-DDNAMN2                                   
013700     MOVE 'Q4 '      TO POSTSUM-TRANSTYP                                  
013800     CALL POSTSUM USING POSTSUM-PARM                                      
013823     .                                                                    
013824     EJECT                                                                
013825 IMS-GN-WDQ4 SECTION.                                                     
013826                                                                          
013827     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-AREA                           
013828     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
013829     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
013830     PERFORM IMS-STATUSKONTROLL                                           
013840     .                                                                    
013850     SKIP3                                                                
013860 IMS-STATUSKONTROLL SECTION.                                              
013870                                                                          
013880     SET STATUS-IX TO 1                                                   
013890     SEARCH GODK-STATUS                                                   
013900       AT END                                                             
014000         CALL FELLOG                                                      
014100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
014200         CONTINUE                                                         
014300     END-SEARCH                                                           
014400     .                                                                    
