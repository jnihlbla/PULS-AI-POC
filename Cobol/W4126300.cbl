000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4126300.                                                
000400*AUTHOR.         LASSI OLGRENER.                                          
000500*DATE-WRITTEN.   93/08/27.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKRIVER FILER MED S-TRANSAR SOM LIGGER KVAR I DISPATCHEN         
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDP8 MED SB                                
001300                                                                          
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*          --- UTFIL S-TRANSAR TILL MAIL (SUPPORT)                        
002100     SELECT W41265                     ASSIGN TO W41263D1.                
002200*          --- UTFIL TACDIS-DLET TILL HENKE                               
002300     SELECT W41263                     ASSIGN TO W41263D2.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600                                                                          
002700 FILE SECTION.                                                            
002800                                                                          
002900 FD  W41265                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200     SKIP2                                                                
003300 01  UTPOST                      PIC X(80).                               
003400     EJECT                                                                
003500 FD  W41263                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900 01  UTPOST1                     PIC X(80).                               
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4126300'.            
004600 01  W-DAGENS-DATUM              PIC 9(6)    VALUE ZERO.                  
004700                                                                          
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 01  DYNAMISKA-SUBPROGRAM.                                                
005200*                                                                         
005300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005600     EJECT                                                                
005700*    --- PARAMETRAR TILL POSTSUM                                          
005800*01  -COPY W0005   -PRE POSTSUM-                                          
005900     EJECT                                                                
006000 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
006100 01  UT-AREA.                                                             
006200     03 UT-IDSNDNOD              PIC X(8).                                
006300     03 FILLER                   PIC X(1)    VALUE SPACE.                 
006400     03 UT-TIREGDAT              PIC 9(6).                                
006500     03 FILLER                   PIC X(1)    VALUE SPACE.                 
006600     03 UT-TIKLOCK               PIC 9(8).                                
006700     03 FILLER                   PIC X(1)    VALUE SPACE.                 
006800     03 UT-IDUSER                PIC X(8).                                
006900     03 FILLER                   PIC X(1)    VALUE SPACE.                 
007000     03 UT-IDMFSMED              PIC X(3).                                
007100     03 FILLER                   PIC X(1)    VALUE SPACE.                 
007200     03 UT-KDKOMSTA              PIC X(1).                                
007300     03 FILLER                   PIC X(2)    VALUE SPACE.                 
007400     03 UT-KDKOMBEH              PIC X(1).                                
007500     03 FILLER                   PIC X(38)   VALUE SPACE.                 
007600     EJECT                                                                
007610 01  FILLER                      PIC X(16)   VALUE 'UT1-AREA'.            
007620 01  UT1-AREA.                                                            
007630     03 UT1-IDSNDNOD             PIC X(8).                                
007640     03 FILLER                   PIC X(1)    VALUE SPACE.                 
007650     03 UT1-TIREGDAT             PIC 9(6).                                
007660     03 FILLER                   PIC X(1)    VALUE SPACE.                 
007670     03 UT1-TIKLOCK              PIC 9(8).                                
007680     03 FILLER                   PIC X(1)    VALUE SPACE.                 
007690     03 UT1-IDMFSMED             PIC X(3).                                
007691     03 FILLER                   PIC X(1)    VALUE SPACE.                 
007693     03 UT1-IDDISTR              PIC X(4).                                
007694     03 FILLER                   PIC X(1)    VALUE SPACE.                 
007695     03 UT1-IDKUNDNR             PIC X(6).                                
007696     03 FILLER                   PIC X(1)    VALUE SPACE.                 
007697     03 UT1-IDORDNR              PIC X(7).                                
007698     03 FILLER                   PIC X(34)   VALUE SPACE.                 
007699     EJECT                                                                
007700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800*                                                                         
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000                                                                          
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008500     88  BASEN-SLUT                          VALUE 'GB'.                  
008600     SKIP2                                                                
008700 01  GODK-STATUSKODER.                                                    
008800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008900     EJECT                                                                
009000*    --- IMS FUNKTIONSKODER                                               
009100*01  -COPY W0003                                                          
009200     EJECT                                                                
009300*    ---  DLI INPUT-OUTPUT AREA                                           
009400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009500 01  DLI-IO-AREA.                                                         
009600   03  -COPY WDP811                                                       
009610   03  -COPY WDP801 -RED TRAN-WDP811                                      
009700     EJECT                                                                
009800 LINKAGE SECTION.                                                         
009900 01  -COPY W0008  -PRE WDP8-                                              
010000     05  FILLER                  PIC X.                                   
010100     EJECT                                                                
010200 PROCEDURE DIVISION  USING WDP8-PCB.                                      
010300     ENTRY 'DLITCBL' USING WDP8-PCB.                                      
010400                                                                          
010500     OPEN OUTPUT W41265 W41263                                            
010600                                                                          
010700     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
010800     MOVE SPACE              TO UT-AREA UT1-AREA                          
010900     ACCEPT W-DAGENS-DATUM   FROM DATE                                    
011000                                                                          
011100     PERFORM IMS-GN-WDP8                                                  
011200     PERFORM UNTIL BASEN-SLUT                                             
011300       EVALUATE WDP8-SEG-NAME-FB                                          
011400         WHEN 'WDP801'                                                    
011500           IF KOM-KDKOMSTA = 'S' AND                                      
011600              W-DAGENS-DATUM > KOM-TIREGDAT                               
011700** DESSA S-TRANSAR SKICKAS VIA MAIL TILL MA-SUPPORT!                      
011800** FÖR ATT UNDVIKA ATT ABENDADE TRANSAR GLÖMS I DISPATCHEN!               
011900                                                                          
012000             MOVE KOM-IDSNDNOD TO UT-IDSNDNOD                             
012100             MOVE KOM-TIREGDAT TO UT-TIREGDAT                             
012200             MOVE KOM-TIKLOCK  TO UT-TIKLOCK                              
012300             MOVE KOM-IDUSER   TO UT-IDUSER                               
012400             MOVE KOM-IDMFSMED TO UT-IDMFSMED                             
012500             MOVE KOM-KDKOMSTA TO UT-KDKOMSTA                             
012600             MOVE KOM-KDKOMBEH TO UT-KDKOMBEH                             
012700                                                                          
012800             WRITE UTPOST FROM UT-AREA                                    
012900             MOVE 'W41265'   TO POSTSUM-FDNAMN                            
013000             MOVE 'W41263D1' TO POSTSUM-DDNAMN2                           
013100             MOVE 'UT'       TO POSTSUM-TRANSTYP                          
013200             CALL POSTSUM USING POSTSUM-PARM                              
013300           END-IF                                                         
013400           IF KOM-IDSNDNOD = 'TACDDLET' AND                               
013410              KOM-IDMFSMED = '067'                                        
013420             MOVE KOM-IDSNDNOD TO UT1-IDSNDNOD                            
013430             MOVE KOM-TIREGDAT TO UT1-TIREGDAT                            
013440             MOVE KOM-TIKLOCK  TO UT1-TIKLOCK                             
013460             MOVE KOM-IDMFSMED TO UT1-IDMFSMED                            
013490           END-IF                                                         
013491         WHEN 'WDP811'                                                    
013492           IF UT1-IDMFSMED = '067'                                        
013493             MOVE TRAN-TRANSDATA(18:4) TO UT1-IDDISTR                     
013494             MOVE TRAN-TRANSDATA(22:6) TO UT1-IDKUNDNR                    
013495             MOVE TRAN-TRANSDATA(28:7) TO UT1-IDORDNR                     
013496             WRITE UTPOST1 FROM UT1-AREA                                  
013497             MOVE 'W41263'   TO POSTSUM-FDNAMN                            
013498             MOVE 'W41263D1' TO POSTSUM-DDNAMN2                           
013499             MOVE 'UT1'      TO POSTSUM-TRANSTYP                          
013500             CALL POSTSUM USING POSTSUM-PARM                              
013501             MOVE SPACE      TO UT1-AREA                                  
013502           END-IF                                                         
013510       END-EVALUATE                                                       
013600       PERFORM IMS-GN-WDP8                                                
013700     END-PERFORM                                                          
013800                                                                          
013900     CLOSE W41265 W41263                                                  
014000     MOVE 'S' TO POSTSUM-OPKOD                                            
014100     CALL POSTSUM USING POSTSUM-PARM                                      
014200     MOVE ZERO TO RETURN-CODE                                             
014300     GOBACK                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 IMS-GN-WDP8 SECTION.                                                     
014700                                                                          
014800     CALL CBLTDLI USING GN WDP8-PCB DLI-IO-AREA                           
014900     MOVE WDP8-STATUS-CODE TO STATUS-WS                                   
015000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015100     PERFORM IMS-STATUSKONTROLL                                           
015200     .                                                                    
015300     SKIP3                                                                
015400 IMS-STATUSKONTROLL SECTION.                                              
015500                                                                          
015600     SET STATUS-IX TO 1                                                   
015700     SEARCH GODK-STATUS                                                   
015800       AT END                                                             
015900         CALL FELLOG                                                      
016000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016100         CONTINUE                                                         
016200     END-SEARCH                                                           
016300     .                                                                    
