000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4267000.                                                
000400*AUTHOR.         ANN WESTBERG.                                            
000500*DATE-WRITTEN.   92/11/16.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
000910*        INDATA: W42668 - R32-RAPP. INLEV. SOM ÄR ÄLDRE                   
000911*                         ÄN 30 ARBETSDAGAR.                              
000920*                                                                         
000930*        UTDATA: W42670 - HISTORIKREGISTER                                
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
002401     SKIP2                                                                
002403     SELECT W42668                     ASSIGN TO W42670D1.                
002410     SELECT W42670                     ASSIGN TO W42670D2.                
002600                                                                          
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W42668                                                               
003003     RECORDING       V                                                    
003004     BLOCK CONTAINS  0.                                                   
003005     SKIP2                                                                
003006*01  -COPY W4266802     -L.                                               
003007     SKIP2                                                                
003008*01  -COPY W4266801     -L.                                               
003009     SKIP2                                                                
003010*01  -COPY W4266803     -L.                                               
003011     SKIP3                                                                
003012 FD  W42670                                                               
003013     RECORDING       V                                                    
003014     BLOCK CONTAINS  0.                                                   
003015     SKIP2                                                                
003016*01  POST -COPY W4266801  -PRE UT01-  -L.                                 
003017     SKIP2                                                                
003018*01  POST -COPY W4266802  -PRE UT02-  -L.                                 
003019     SKIP2                                                                
003020*01  POST -COPY W4266803  -PRE UT03-  -L.                                 
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300     SKIP2                                                                
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W4267000'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W42668-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W42668                       VALUE 'J'.                   
003900     EJECT                                                                
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500     EJECT                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     SKIP2                                                                
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005901     EJECT                                                                
005902*    --- PARAMETRAR TILL POSTSUM                                          
005903*                                                                         
005910*01  -COPY W0005   -PRE  POSTSUM-                                         
006101     EJECT                                                                
006102 01  IN-AREA-START               PIC X(24)   VALUE                        
006103                                 'IN-AREA-START  '.                       
006104 01  IN-AREA.                                                             
006105     03  IN-IDPTYP               PIC X(3).                                
006106     03  FILLER                  PIC X(290).                              
006111*01  FILLER -COPY W4266801      -PRE IN01- -RED  IN-AREA                  
006112*01  FILLER -COPY W4266802      -PRE IN02- -RED  IN-AREA                  
006113*01  FILLER -COPY W4266803      -PRE IN03- -RED  IN-AREA                  
006114                                                                          
006115     EJECT                                                                
006116 01  UT-AREA-START               PIC X(24)   VALUE                        
006117                                 'UT-AREA-START  '.                       
006118                                                                          
006119 01  UT-AREA.                                                             
006120*    03  UT-IDPTYP               PIC X(3).                                
006121     03  FILLER                  PIC X(290).                              
006122*01  FILLER -COPY W4266801      -PRE UT01- -RED  UT-AREA                  
006123*01  FILLER -COPY W4266802      -PRE UT02- -RED  UT-AREA                  
006130*01  FILLER -COPY W4266803      -PRE UT03- -RED  UT-AREA                  
006200     EJECT                                                                
006300 PROCEDURE DIVISION.                                                      
006600                                                                          
006700     PERFORM A-INIT                                                       
006810     PERFORM S01-LAES-W42668                                              
006900     PERFORM UNTIL END-OF-W42668                                          
006910       IF IN-IDPTYP = '101'                                               
007000         PERFORM S11-SKRIV-W42670-101                                     
007100       END-IF                                                             
007200       IF IN-IDPTYP = '111'                                               
007300         PERFORM S11-SKRIV-W42670-111                                     
007400       END-IF                                                             
007500       IF IN-IDPTYP = '112'                                               
007600         PERFORM S11-SKRIV-W42670-112                                     
007601       END-IF                                                             
007610       PERFORM S01-LAES-W42668                                            
007700     END-PERFORM                                                          
007900                                                                          
008000     PERFORM Z-FINIT                                                      
008100                                                                          
008200     MOVE ZERO TO RETURN-CODE                                             
008300     GOBACK                                                               
008400     .                                                                    
008500     EJECT                                                                
008600 A-INIT SECTION.                                                          
008701                                                                          
008710     OPEN INPUT  W42668                                                   
008801                                                                          
008810     OPEN OUTPUT W42670                                                   
008820                                                                          
009000     ACCEPT DAGENS-DATUM  FROM DATE                                       
009110     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009200     .                                                                    
009300     EJECT                                                                
009802 S01-LAES-W42668  SECTION.                                                
009803                                                                          
009804     READ W42668 INTO IN-AREA                                             
009805     AT END                                                               
009806        MOVE JA TO W42668-EOF-SW                                          
009808                                                                          
009809     NOT AT END                                                           
009810        MOVE 'W42668' TO POSTSUM-FDNAMN                                   
009811        MOVE 'W42670D1' TO POSTSUM-DDNAMN2                                
009813        CALL POSTSUM USING POSTSUM-PARM                                   
009814     END-READ                                                             
009820     .                                                                    
009901     EJECT                                                                
009902 S11-SKRIV-W42670-101 SECTION.                                            
009903     MOVE IN01-IDPTYP             TO UT01-IDPTYP                          
009904     MOVE IN01-IDLOPNRM           TO UT01-IDLOPNRM                        
009905     MOVE IN01-ADKVAULG           TO UT01-ADKVAULG                        
009906     MOVE IN01-BEANST             TO UT01-BEANST                          
009907     MOVE IN01-FLANNULL           TO UT01-FLANNULL                        
009908     MOVE IN01-FLKVARED           TO UT01-FLKVARED                        
009909     MOVE IN01-FLSKPSAK           TO UT01-FLSKPSAK                        
009910     MOVE IN01-FLKVAUTV-ANT       TO UT01-FLKVAUTV-ANT                    
009911     MOVE IN01-FLKVAUTV-KVAL      TO UT01-FLKVAUTV-KVAL                   
009912     MOVE IN01-IDARTNR            TO UT01-IDARTNR                         
009913     MOVE IN01-IDLEVNR            TO UT01-IDLEVNR                         
009914     MOVE IN01-IDUSER-PRI         TO UT01-IDUSER-PRI                      
009915     MOVE IN01-IDUSER-SEK         TO UT01-IDUSER-SEK                      
009916     MOVE IN01-KDKVATYP           TO UT01-KDKVATYP                        
009917     MOVE IN01-IDPROVPL-PRI       TO UT01-IDPROVPL-PRI                    
009918     MOVE IN01-IDPROVPL-SEK       TO UT01-IDPROVPL-SEK                    
009919     MOVE IN01-KDKVAULG           TO UT01-KDKVAULG                        
009920     MOVE IN01-KDKVASTA-ANT       TO UT01-KDKVASTA-ANT                    
009921     MOVE IN01-KDKVASTA-PRI       TO UT01-KDKVASTA-PRI                    
009922     MOVE IN01-KDKVASTA-SEK       TO UT01-KDKVASTA-SEK                    
009923     MOVE IN01-KVKVAPRIM          TO UT01-KVKVAPRIM                       
009924     MOVE IN01-KVKVASEK           TO UT01-KVKVASEK                        
009925     MOVE IN01-TIREGDAT           TO UT01-TIREGDAT                        
009926                                                                          
009927                                                                          
009928     WRITE UT01-POST FROM UT01-W4266801                                   
009929                                                                          
009930     MOVE 'W42670' TO POSTSUM-FDNAMN                                      
009931     MOVE 'W42670D2' TO POSTSUM-DDNAMN2                                   
009932     CALL POSTSUM USING POSTSUM-PARM                                      
009940     .                                                                    
010100     EJECT                                                                
010101 S11-SKRIV-W42670-111 SECTION.                                            
010102                                                                          
010103     MOVE IN02-IDPTYP             TO UT02-IDPTYP                          
010104     MOVE IN02-IDKR               TO UT02-IDKR                            
010105     MOVE IN02-BEKRFEL(1)         TO UT02-BEKRFEL(1)                      
010106     MOVE IN02-BEKRFEL(2)         TO UT02-BEKRFEL(2)                      
010107     MOVE IN02-BEKRFEL(3)         TO UT02-BEKRFEL(3)                      
010108     MOVE IN02-KDKVASTA-PRI       TO UT02-KDKVASTA-PRI                    
010109     MOVE IN02-TEKRFEL            TO UT02-TEKRFEL                         
010110                                                                          
010111     WRITE UT02-POST FROM UT02-W4266802                                   
010112                                                                          
010114     MOVE 'W42670' TO POSTSUM-FDNAMN                                      
010115     MOVE 'W42670D2' TO POSTSUM-DDNAMN2                                   
010116     CALL POSTSUM USING POSTSUM-PARM                                      
010117     .                                                                    
010118     EJECT                                                                
010119 S11-SKRIV-W42670-112 SECTION.                                            
010120                                                                          
010121     MOVE IN03-IDPTYP             TO UT03-IDPTYP                          
010122     MOVE IN03-IDKVAINF           TO UT03-IDKVAINF                        
010123     MOVE IN03-KDKVASTA-PRI       TO UT03-KDKVASTA-PRI                    
010124     MOVE IN03-TEKVAINF(1)        TO UT03-TEKVAINF(1)                     
010125     MOVE IN03-TEKVAINF(2)        TO UT03-TEKVAINF(2)                     
010126     MOVE IN03-TEKVAINF(3)        TO UT03-TEKVAINF(3)                     
010127                                                                          
010128     WRITE UT03-POST FROM UT03-W4266803                                   
010129                                                                          
010131     MOVE 'W42670' TO POSTSUM-FDNAMN                                      
010132     MOVE 'W42670D2' TO POSTSUM-DDNAMN2                                   
010133     CALL POSTSUM USING POSTSUM-PARM                                      
010134     .                                                                    
010135     EJECT                                                                
010136 Z-FINIT SECTION.                                                         
010137                                                                          
010138     CLOSE W42668                                                         
010140           W42670                                                         
010150                                                                          
010160     MOVE 'S' TO POSTSUM-OPKOD                                            
010170     CALL POSTSUM USING POSTSUM-PARM                                      
010180     .                                                                    
010190     EJECT                                                                
