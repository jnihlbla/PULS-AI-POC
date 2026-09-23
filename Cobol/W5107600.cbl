000100 ID DIVISION.                                                             
000202 PROGRAM-ID.     W5107600.                                                
000301 AUTHOR.         BARSHARANI BISHOYE.                                      
000401 DATE-WRITTEN.   19052021.                                                
000501*    REMARKS.                                                             
000601*                SKAPAR HÄNDELSELISTOR TILL ON-DEMAND                     
000701*                                                                         
000802*        INDATA  W5107Q W5107R                                            
000901                                                                          
001001*                                                                         
001101     EJECT                                                                
001201                                                                          
001301 ENVIRONMENT DIVISION.                                                    
001401                                                                          
001501 INPUT-OUTPUT SECTION.                                                    
001601                                                                          
001701 FILE-CONTROL.                                                            
001802     SELECT W5107Q              ASSIGN TO W51076D1.                       
002102     SELECT W5107R              ASSIGN TO W51076D2.                       
002202     SELECT W5107S              ASSIGN TO W51076D3.                       
002203     SELECT W5107T              ASSIGN TO W51076D4.                       
002204     SELECT W5107QA             ASSIGN TO W51076D5.                       
002205     SELECT W5107RA             ASSIGN TO W51076D6.                       
002300     EJECT                                                                
002400                                                                          
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700                                                                          
002802 FD  W5107Q                                                               
002900     RECORDING V                                                          
003000     BLOCK CONTAINS 0.                                                    
003100 01  FILLER                       PIC X(165).                             
003200                                                                          
004301 FD  W5107R                                                               
004401     RECORDING V                                                          
004501     BLOCK CONTAINS 0.                                                    
004601 01 FILLER                        PIC X(165).                             
004701                                                                          
004801 FD  W5107S                                                               
004901     RECORDING V                                                          
005001     BLOCK CONTAINS 0.                                                    
005101 01  LISTPOST1                    PIC X(20).                              
005102                                                                          
005103 FD  W5107T                                                               
005104     RECORDING V                                                          
005105     BLOCK CONTAINS 0.                                                    
005106 01  LISTPOST2                    PIC X(20).                              
005200     EJECT                                                                
005210 FD  W5107QA                                                              
005220     RECORDING V                                                          
005230     BLOCK CONTAINS 0.                                                    
005240 01  LISTPOST3                    PIC X(165).                             
005250     EJECT                                                                
005260 FD  W5107RA                                                              
005270     RECORDING V                                                          
005280     BLOCK CONTAINS 0.                                                    
005290 01  LISTPOST4                    PIC X(165).                             
005291     EJECT                                                                
005300                                                                          
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005602 77  IDPGM               PIC X(8)        VALUE 'W5107600'.                
005700 77  JA                  PIC X           VALUE 'J'.                       
005800 77  NEJ                 PIC X           VALUE 'N'.                       
005900 77  IN-EOF              PIC X           VALUE 'N'.                       
005910 77  IN1-EOF             PIC X           VALUE 'N'.                       
006000                                                                          
006400 01  W-IDVERGL           PIC X(10)       VALUE SPACE.                     
006500 01  WS-IDVERGL          PIC X(10)       VALUE SPACE.                     
006520 01  W-IDDC              PIC X(2)        VALUE SPACE.                     
006530 01  W1-IDDC              PIC X(2)        VALUE SPACE.                    
006800 01  W-DATUM-T1          PIC X(8)        VALUE SPACE.                     
006900 01  W-TID-T1            PIC X(4)        VALUE SPACE.                     
007000 01  W-DATUM-T2          PIC X(8)        VALUE SPACE.                     
007100 01  W-TID-T2            PIC X(4)        VALUE SPACE.                     
007110 01  FIRST-WRITE         PIC X           VALUE 'Y'.                       
007120 01  SECOND-WRITE        PIC X           VALUE 'Y'.                       
007121 01  FIRST-WRITE1        PIC X           VALUE 'Y'.                       
007122 01  SECOND-WRITE1       PIC X           VALUE 'Y'.                       
007130 01  W-DC-CHECK          PIC X           VALUE 'Y'.                       
007140 01  W1-DC-CHECK         PIC X           VALUE 'Y'.                       
007200     EJECT                                                                
007210*01  -COPY WWDC99                                                         
007230 01  IN-AREA.                                                             
007240     03  FILLER                 PIC X(02)    VALUE SPACE.                 
007250     03  IN-IDVERGL             PIC X(10)    VALUE SPACE.                 
007260     03  FILLER                 PIC X(01)    VALUE ';'.                   
007270     03  IN-DATUM-T1            PIC X(8)     VALUE SPACE.                 
007280     03  FILLER                 PIC X(01)    VALUE ';'.                   
007290     03  IN-DAVERDAT            PIC X(10)    VALUE SPACE.                 
007291     03  FILLER                 PIC X(01)    VALUE ';'.                   
007292     03  IN-HAENDELSE           PIC X(07).                                
007293     03  FILLER REDEFINES IN-HAENDELSE.                                   
007294         05  IN-KDEKHHT         PIC X(03).                                
007295         05  IN-STRECK          PIC X(01).                                
007296         05  IN-KDEKSHT         PIC X(03).                                
007298     03  FILLER                 PIC X(01)    VALUE ';'.                   
007299     03  IN-KDEKNIVA            PIC X(04)    VALUE SPACE.                 
007300     03  FILLER                 PIC X(01)    VALUE ';'.                   
007310     03  IN-KDDOKTYP            PIC X(02)    VALUE SPACE.                 
007320     03  FILLER                 PIC X(01)    VALUE ';'.                   
007330     03  IN-IDDC                PIC X(02)    VALUE SPACE.                 
007340     03  FILLER                 PIC X(01)    VALUE ';'.                   
007350     03  IN-KVANTAL             PIC X(07)    VALUE SPACE.                 
007390     03  FILLER                 PIC X(01)    VALUE ';'.                   
007391     03  IN-KVANTAL-TKN         PIC X(1)     VALUE SPACE.                 
007392     03  FILLER                 PIC X(01)    VALUE ';'.                   
007393     03  IN-IDARTNR             PIC Z(8)9    VALUE ZERO.                  
007394     03  FILLER                 PIC X(01)    VALUE ';'.                   
007395     03  IN-KDPRODSL            PIC Z(3)     VALUE ZERO.                  
007396     03  FILLER                 PIC X(01)    VALUE ';'.                   
007397     03  IN-IDKONTO             PIC 9(10)    VALUE ZERO.                  
007398     03  FILLER                 PIC X(01)    VALUE ';'.                   
007399     03  IN-KDPOST              PIC X(02)    VALUE SPACE.                 
007400     03  FILLER                 PIC X(01)    VALUE ';'.                   
007410     03  IN-IDKST               PIC X(10)    VALUE SPACE.                 
007420     03  FILLER                 PIC X(01)    VALUE ';'.                   
007430     03  IN-IDANALYS            PIC X(12)    VALUE SPACE.                 
007440     03  FILLER                 PIC X(01)    VALUE ';'.                   
007450     03  IN-SUBEL               PIC Z(8)9.99 VALUE ZERO.                  
007460     03  FILLER                 PIC X(01)    VALUE ';'.                   
007470     03  IN-TECKEN              PIC X(01)    VALUE SPACE.                 
007480     03  FILLER                 PIC X(01)    VALUE ';'.                   
007490     03  IN-IDPRCTR             PIC X(10)    VALUE SPACE.                 
007491     03  FILLER                 PIC X(01)    VALUE ';'.                   
007492     EJECT                                                                
007493 01  IN1-AREA.                                                            
007494     03  FILLER                 PIC X(02)    VALUE SPACE.                 
007495     03  IN1-IDVERGL            PIC X(10)    VALUE SPACE.                 
007496     03  FILLER                 PIC X(01)    VALUE ';'.                   
007497     03  IN1-DATUM-T1           PIC X(8)     VALUE SPACE.                 
007498     03  FILLER                 PIC X(01)    VALUE ';'.                   
007499     03  IN1-DAVERDAT           PIC X(10)    VALUE SPACE.                 
007500     03  FILLER                 PIC X(01)    VALUE ';'.                   
007510     03  IN1-HAENDELSE           PIC X(07).                               
007520     03  FILLER REDEFINES IN1-HAENDELSE.                                  
007530         05  IN1-KDEKHHT        PIC X(03).                                
007540         05  IN1-STRECK         PIC X(01).                                
007550         05  IN1-KDEKSHT        PIC X(03).                                
007570     03  FILLER                 PIC X(01)    VALUE ';'.                   
007580     03  IN1-KDEKNIVA           PIC X(04)    VALUE SPACE.                 
007590     03  FILLER                 PIC X(01)    VALUE ';'.                   
007591     03  IN1-KDDOKTYP           PIC X(02)    VALUE SPACE.                 
007592     03  FILLER                 PIC X(01)    VALUE ';'.                   
007593     03  IN1-IDDC               PIC X(02)    VALUE SPACE.                 
007594     03  FILLER                 PIC X(01)    VALUE ';'.                   
007595     03  IN1-KVANTAL            PIC X(07)    VALUE SPACE.                 
007599     03  FILLER                 PIC X(01)    VALUE ';'.                   
007600     03  IN1-KVANTAL-TKN        PIC X(1)     VALUE SPACE.                 
007610     03  FILLER                 PIC X(01)    VALUE ';'.                   
007620     03  IN1-IDARTNR            PIC Z(8)9    VALUE ZERO.                  
007630     03  FILLER                 PIC X(01)    VALUE ';'.                   
007640     03  IN1-KDPRODSL           PIC Z(3)     VALUE ZERO.                  
007650     03  FILLER                 PIC X(01)    VALUE ';'.                   
007660     03  IN1-IDKONTO            PIC 9(10)    VALUE ZERO.                  
007670     03  FILLER                 PIC X(01)    VALUE ';'.                   
007680     03  IN1-KDPOST             PIC X(02)    VALUE SPACE.                 
007690     03  FILLER                 PIC X(01)    VALUE ';'.                   
007691     03  IN1-IDKST              PIC X(10)    VALUE SPACE.                 
007692     03  FILLER                 PIC X(01)    VALUE ';'.                   
007693     03  IN1-IDANALYS           PIC X(12)    VALUE SPACE.                 
007694     03  FILLER                 PIC X(01)    VALUE ';'.                   
007695     03  IN1-SUBEL              PIC Z(8)9.99 VALUE ZERO.                  
007696     03  FILLER                 PIC X(01)    VALUE ';'.                   
007697     03  IN1-TECKEN             PIC X(01)    VALUE SPACE.                 
007698     03  FILLER                 PIC X(01)    VALUE ';'.                   
007699     03  IN1-IDPRCTR            PIC X(10)    VALUE SPACE.                 
007700     03  FILLER                 PIC X(01)    VALUE ';'.                   
007710     EJECT                                                                
007800                                                                          
025001 01  UT1-AREA.                                                            
025002     03  FILLER                 PIC X(02)    VALUE SPACE.                 
025201     03  UT1-IDVERGL             PIC X(10)    VALUE SPACE.                
025301     03  FILLER                 PIC X(01)    VALUE ';'.                   
025302     03  UT1-IDDC                PIC X(2)     VALUE SPACE.                
029602     03  FILLER                 PIC X(01)    VALUE ';'.                   
029603     EJECT                                                                
029701                                                                          
029702 01  UT2-AREA.                                                            
029703     03  FILLER                 PIC X(02)    VALUE SPACE.                 
029704     03  UT2-IDVERGL             PIC X(10)    VALUE SPACE.                
029705     03  FILLER                 PIC X(01)    VALUE ';'.                   
029706     03  UT2-IDDC                PIC X(2)     VALUE SPACE.                
029707     03  FILLER                 PIC X(01)    VALUE ';'.                   
029708     EJECT                                                                
029709 01  UT3-AREA.                                                            
029752     03  FILLER                 PIC X(02)    VALUE SPACE.                 
029753     03  UT3-IDVERGL             PIC X(10)    VALUE SPACE.                
029754     03  FILLER                 PIC X(01)    VALUE ';'.                   
029755     03  UT3-DATUM-T1             PIC X(8)     VALUE SPACE.               
029756     03  FILLER                 PIC X(01)    VALUE ';'.                   
029757     03  UT3-DAVERDAT            PIC X(10)    VALUE SPACE.                
029758     03  FILLER                 PIC X(01)    VALUE ';'.                   
029759     03  UT3-HAENDELSE           PIC X(07).                               
029760     03  FILLER REDEFINES UT3-HAENDELSE.                                  
029761         05  UT3-KDEKHHT         PIC X(03).                               
029762         05  UT3-STRECK          PIC X(01).                               
029763         05  UT3-KDEKSHT         PIC X(03).                               
029764     03  FILLER                 PIC X(01)    VALUE ';'.                   
029765     03  UT3-KDEKNIVA            PIC X(04)    VALUE SPACE.                
029766     03  FILLER                 PIC X(01)    VALUE ';'.                   
029767     03  UT3-KDDOKTYP            PIC X(02)    VALUE SPACE.                
029768     03  FILLER                 PIC X(01)    VALUE ';'.                   
029769     03  UT3-IDDC                PIC X(02)    VALUE SPACE.                
029770     03  FILLER                 PIC X(01)    VALUE ';'.                   
029771     03  UT3-KVANTAL             PIC X(07)    VALUE SPACE.                
029772     03  FILLER                 PIC X(01)    VALUE ';'.                   
029773     03  UT3-KVANTAL-TKN         PIC X(1)     VALUE SPACE.                
029774     03  FILLER                 PIC X(01)    VALUE ';'.                   
029775     03  UT3-IDARTNR             PIC Z(8)9    VALUE ZERO.                 
029776     03  FILLER                 PIC X(01)    VALUE ';'.                   
029777     03  UT3-KDPRODSL            PIC Z(3)     VALUE ZERO.                 
029778     03  FILLER                 PIC X(01)    VALUE ';'.                   
029779     03  UT3-IDKONTO             PIC 9(10)    VALUE ZERO.                 
029780     03  FILLER                 PIC X(01)    VALUE ';'.                   
029781     03  UT3-KDPOST              PIC X(02)    VALUE SPACE.                
029782     03  FILLER                 PIC X(01)    VALUE ';'.                   
029783     03  UT3-IDKST               PIC X(10)    VALUE SPACE.                
029784     03  FILLER                 PIC X(01)    VALUE ';'.                   
029785     03  UT3-IDANALYS            PIC X(12)    VALUE SPACE.                
029786     03  FILLER                 PIC X(01)    VALUE ';'.                   
029787     03  UT3-SUBEL               PIC Z(8)9.99 VALUE ZERO.                 
029788     03  FILLER                 PIC X(01)    VALUE ';'.                   
029789     03  UT3-TECKEN              PIC X(01)    VALUE SPACE.                
029790     03  FILLER                 PIC X(01)    VALUE ';'.                   
029791     03  UT3-IDPRCTR             PIC X(10)    VALUE SPACE.                
029792     03  FILLER                 PIC X(01)    VALUE ';'.                   
029793     EJECT                                                                
029794 01  UT4-AREA.                                                            
029795     03  FILLER                 PIC X(02)    VALUE SPACE.                 
029796     03  UT4-IDVERGL             PIC X(10)    VALUE SPACE.                
029797     03  FILLER                 PIC X(01)    VALUE ';'.                   
029800     03  UT4-DATUM-T1             PIC X(8)     VALUE SPACE.               
029810     03  FILLER                 PIC X(01)    VALUE ';'.                   
029820     03  UT4-DAVERDAT            PIC X(10)    VALUE SPACE.                
029821     03  FILLER                 PIC X(01)    VALUE ';'.                   
029822     03  UT4-HAENDELSE           PIC X(07).                               
029823     03  FILLER REDEFINES UT4-HAENDELSE.                                  
029824         05  UT4-KDEKHHT         PIC X(03).                               
029825         05  UT4-STRECK          PIC X(01).                               
029826         05  UT4-KDEKSHT         PIC X(03).                               
029827     03  FILLER                 PIC X(01)    VALUE ';'.                   
029828     03  UT4-KDEKNIVA            PIC X(04)    VALUE SPACE.                
029829     03  FILLER                 PIC X(01)    VALUE ';'.                   
029830     03  UT4-KDDOKTYP            PIC X(02)    VALUE SPACE.                
029831     03  FILLER                 PIC X(01)    VALUE ';'.                   
029832     03  UT4-IDDC                PIC X(02)    VALUE SPACE.                
029833     03  FILLER                 PIC X(01)    VALUE ';'.                   
029834     03  UT4-KVANTAL             PIC X(07)    VALUE SPACE.                
029835     03  FILLER                 PIC X(01)    VALUE ';'.                   
029836     03  UT4-KVANTAL-TKN         PIC X(1)     VALUE SPACE.                
029837     03  FILLER                 PIC X(01)    VALUE ';'.                   
029838     03  UT4-IDARTNR             PIC Z(8)9    VALUE ZERO.                 
029839     03  FILLER                 PIC X(01)    VALUE ';'.                   
029840     03  UT4-KDPRODSL            PIC Z(3)     VALUE ZERO.                 
029841     03  FILLER                 PIC X(01)    VALUE ';'.                   
029842     03  UT4-IDKONTO             PIC 9(10)    VALUE ZERO.                 
029843     03  FILLER                 PIC X(01)    VALUE ';'.                   
029844     03  UT4-KDPOST              PIC X(02)    VALUE SPACE.                
029845     03  FILLER                 PIC X(01)    VALUE ';'.                   
029846     03  UT4-IDKST               PIC X(10)    VALUE SPACE.                
029847     03  FILLER                 PIC X(01)    VALUE ';'.                   
029848     03  UT4-IDANALYS            PIC X(12)    VALUE SPACE.                
029849     03  FILLER                 PIC X(01)    VALUE ';'.                   
029850     03  UT4-SUBEL               PIC Z(8)9.99 VALUE ZERO.                 
029851     03  FILLER                 PIC X(01)    VALUE ';'.                   
029852     03  UT4-TECKEN              PIC X(01)    VALUE SPACE.                
029853     03  FILLER                 PIC X(01)    VALUE ';'.                   
029854     03  UT4-IDPRCTR             PIC X(10)    VALUE SPACE.                
029855     03  FILLER                 PIC X(01)    VALUE ';'.                   
029856     EJECT                                                                
029857                                                                          
029860 PROCEDURE DIVISION.                                                      
029900     PERFORM A-INITIERA                                                   
030000                                                                          
030100     PERFORM S01-READ-INFIL                                               
030110     PERFORM S01-READ-INFIL1                                              
030200     PERFORM UNTIL IN-EOF = JA  AND IN1-EOF = JA                          
030210       IF IN-IDVERGL NOT = IN1-IDVERGL                                    
030400         IF IN-EOF = 'N'                                                  
030410           PERFORM B-INVOICE                                              
030500           PERFORM S01-READ-INFIL                                         
030501         END-IF                                                           
030503         IF IN1-EOF = 'N'                                                 
030504           PERFORM BA-INVOICE                                             
030510           PERFORM S01-READ-INFIL1                                        
030511         END-IF                                                           
030512       ELSE                                                               
030513         IF IN-EOF = 'N'                                                  
030514           PERFORM B-INVOICE                                              
030515           PERFORM S01-READ-INFIL                                         
030516         END-IF                                                           
030517         IF IN1-EOF = 'N'                                                 
030518           PERFORM BA-INVOICE                                             
030519           PERFORM S01-READ-INFIL1                                        
030520         END-IF                                                           
030530       END-IF                                                             
030600     END-PERFORM                                                          
030700                                                                          
030800     PERFORM Z-AVSLUTA                                                    
030900     MOVE ZERO TO RETURN-CODE                                             
031000     GOBACK                                                               
031100     .                                                                    
031200                                                                          
031300 A-INITIERA SECTION.                                                      
031402     OPEN INPUT  W5107Q                                                   
031403                 W5107R                                                   
031500          OUTPUT W5107S                                                   
031600                 W5107T                                                   
031610                 W5107QA                                                  
031620                 W5107RA                                                  
031800                                                                          
031900     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DATUM-T1                        
032000                                        W-DATUM-T2                        
032100     MOVE FUNCTION CURRENT-DATE(9:4) TO W-TID-T1                          
032200                                        W-TID-T2                          
032300     .                                                                    
032400     EJECT                                                                
032500                                                                          
032600 B-INVOICE SECTION.                                                       
032710******************************************************************        
032711*WRITING INVOICE WHICH USED DC 61 AND 62 FROM W5107Q FILE *               
032712******************************************************************        
032713     MOVE IN-IDDC               TO WS-IDDC                                
032714     IF NDC-JP                                                            
032715       IF FIRST-WRITE  = 'Y'                                              
037803         MOVE IN-IDVERGL        TO UT1-IDVERGL                            
037804         MOVE IN-IDDC           TO UT1-IDDC                               
037807         PERFORM S02-WRITE-LISTA1                                         
037808         MOVE IN-IDVERGL   TO  W-IDVERGL                                  
037809         MOVE 'N'          TO  FIRST-WRITE                                
037810       ELSE                                                               
037811         IF W-IDVERGL  =  IN-IDVERGL                                      
037812           CONTINUE                                                       
037813         ELSE                                                             
037815           MOVE IN-IDVERGL        TO UT1-IDVERGL                          
037816           MOVE IN-IDDC           TO UT1-IDDC                             
037819           PERFORM S02-WRITE-LISTA1                                       
037820           MOVE IN-IDVERGL   TO  W-IDVERGL                                
037821         END-IF                                                           
037822       END-IF                                                             
037823     ELSE                                                                 
037824       IF NDC-AU                                                          
037825         IF FIRST-WRITE1 = 'Y'                                            
037826           MOVE IN-IDVERGL      TO UT2-IDVERGL                            
037827           MOVE IN-IDDC         TO UT2-IDDC                               
037828           PERFORM S02-WRITE-LISTA2                                       
037829           MOVE IN-IDVERGL TO  W-IDVERGL                                  
037830           MOVE 'N'        TO  FIRST-WRITE1                               
037831         ELSE                                                             
037832           IF W-IDVERGL = IN-IDVERGL                                      
037833             CONTINUE                                                     
037834           ELSE                                                           
037835             MOVE IN-IDVERGL      TO UT2-IDVERGL                          
037836             MOVE IN-IDDC         TO UT2-IDDC                             
037837             PERFORM S02-WRITE-LISTA2                                     
037838             MOVE IN-IDVERGL TO  W-IDVERGL                                
037839           END-IF                                                         
037840         END-IF                                                           
037859       END-IF                                                             
037860     END-IF                                                               
037861*****************************************************************         
037862*EXCLUDING DC 91 AND RELATED RECORD FOR EVENT302-301 FROM W5107Q*         
037863*****************************************************************         
037864     IF  IN-KDEKHHT = '302'                                               
037865     AND IN-KDEKSHT = '301'                                               
037866     AND IN-IDDC   = '91'                                                 
037867       MOVE 'Y' TO W-DC-CHECK                                             
037868     ELSE                                                                 
037869       IF  IN-KDEKHHT = '302'                                             
037870       AND IN-KDEKSHT = '301'                                             
037871       AND W-DC-CHECK  = 'Y'                                              
037872       AND IN-IDDC  = ' '                                                 
037873         CONTINUE                                                         
037874         MOVE 'N' TO W-DC-CHECK                                           
037875       ELSE                                                               
037876         MOVE IN-IDVERGL    TO UT3-IDVERGL                                
037877         MOVE W-DATUM-T1    TO UT3-DATUM-T1                               
037878         MOVE IN-DAVERDAT   TO UT3-DAVERDAT                               
037879         MOVE IN-KDEKHHT    TO UT3-KDEKHHT                                
037880         MOVE '-'           TO UT3-STRECK                                 
037881         MOVE IN-KDEKSHT    TO UT3-KDEKSHT                                
037882         MOVE IN-KDEKNIVA   TO UT3-KDEKNIVA                               
037883         MOVE IN-KDDOKTYP   TO UT3-KDDOKTYP                               
037884         MOVE IN-IDDC       TO UT3-IDDC                                   
037885         MOVE IN-KVANTAL    TO UT3-KVANTAL                                
037886         MOVE IN-KVANTAL-TKN TO UT3-KVANTAL-TKN                           
037887         MOVE IN-IDARTNR    TO UT3-IDARTNR                                
037888         MOVE IN-KDPRODSL   TO UT3-KDPRODSL                               
037889         MOVE IN-IDKONTO    TO UT3-IDKONTO                                
037890         INSPECT UT3-IDKONTO REPLACING LEADING ZERO BY SPACE              
037891         MOVE IN-IDKST      TO UT3-IDKST                                  
037892         MOVE IN-IDANALYS   TO UT3-IDANALYS                               
037893         MOVE IN-SUBEL      TO UT3-SUBEL                                  
037894         MOVE IN-TECKEN     TO UT3-TECKEN                                 
037895         MOVE IN-IDPRCTR    TO UT3-IDPRCTR                                
037896         MOVE IN-KDPOST     TO UT3-KDPOST                                 
037897         IF IN-EOF = 'N'                                                  
037898           PERFORM S02-WRITE-LISTA3                                       
037899         END-IF                                                           
037900       END-IF                                                             
037901     END-IF                                                               
037902     .                                                                    
037903 BA-INVOICE SECTION.                                                      
037904******************************************************************        
037905*WRITING INVOICE WHICH USED DC 61 AND 62 FROM W5107R FILE *               
037906******************************************************************        
037907     MOVE IN1-IDDC               TO WS-IDDC                               
037908     IF NDC-JP                                                            
037909       IF SECOND-WRITE  = 'Y'                                             
037910         MOVE IN1-IDVERGL        TO UT1-IDVERGL                           
037911         MOVE IN1-IDDC           TO UT1-IDDC                              
037912         PERFORM S02-WRITE-LISTA1                                         
037913         MOVE IN1-IDVERGL   TO  WS-IDVERGL                                
037914         MOVE 'N'           TO  SECOND-WRITE                              
037915       ELSE                                                               
037916         IF WS-IDVERGL  =  IN1-IDVERGL                                    
037917           CONTINUE                                                       
037918         ELSE                                                             
037919           MOVE IN1-IDVERGL        TO UT1-IDVERGL                         
037920           MOVE IN1-IDDC           TO UT1-IDDC                            
037921           PERFORM S02-WRITE-LISTA1                                       
037922           MOVE IN1-IDVERGL   TO  WS-IDVERGL                              
037923         END-IF                                                           
037924       END-IF                                                             
037925     ELSE                                                                 
037926       IF NDC-AU                                                          
037927         IF SECOND-WRITE1 = 'Y'                                           
037928           MOVE IN1-IDVERGL      TO UT2-IDVERGL                           
037929           MOVE IN1-IDDC         TO UT2-IDDC                              
037930           PERFORM S02-WRITE-LISTA2                                       
037931           MOVE IN1-IDVERGL TO  WS-IDVERGL                                
037932           MOVE 'N'         TO  SECOND-WRITE1                             
037933         ELSE                                                             
037934           IF WS-IDVERGL = IN1-IDVERGL                                    
037935             CONTINUE                                                     
037936           ELSE                                                           
037937             MOVE IN1-IDVERGL      TO UT2-IDVERGL                         
037938             MOVE IN1-IDDC         TO UT2-IDDC                            
037939             PERFORM S02-WRITE-LISTA2                                     
037940             MOVE IN1-IDVERGL TO  WS-IDVERGL                              
037941           END-IF                                                         
037942         END-IF                                                           
037943       END-IF                                                             
037950     END-IF                                                               
038030*****************************************************************         
038031*EXCLUDING DC 91 AND RELATED RECORD FOR EVENT302-301 FROM W5107R*         
038033*****************************************************************         
038093     IF  IN1-KDEKHHT = '302'                                              
038094     AND IN1-KDEKSHT = '301'                                              
038095     AND IN1-IDDC   = '91'                                                
038096       MOVE 'Y' TO W1-DC-CHECK                                            
038098     ELSE                                                                 
038099       IF  IN1-KDEKHHT = '302'                                            
038100       AND IN1-KDEKSHT = '301'                                            
038101       AND W1-DC-CHECK  = 'Y'                                             
038102       AND IN1-IDDC  = ' '                                                
038103         CONTINUE                                                         
038104         MOVE 'N' TO W1-DC-CHECK                                          
038105       ELSE                                                               
038107         MOVE IN1-IDVERGL    TO UT4-IDVERGL                               
038108         MOVE W-DATUM-T1     TO UT4-DATUM-T1                              
038109         MOVE IN1-DAVERDAT   TO UT4-DAVERDAT                              
038110         MOVE IN1-KDEKHHT    TO UT4-KDEKHHT                               
038111         MOVE '-'            TO UT4-STRECK                                
038112         MOVE IN1-KDEKSHT    TO UT4-KDEKSHT                               
038113         MOVE IN1-KDEKNIVA   TO UT4-KDEKNIVA                              
038114         MOVE IN1-KDDOKTYP   TO UT4-KDDOKTYP                              
038115         MOVE IN1-IDDC       TO UT4-IDDC                                  
038116         MOVE IN1-KVANTAL    TO UT4-KVANTAL                               
038117         MOVE IN1-KVANTAL-TKN TO UT4-KVANTAL-TKN                          
038118         MOVE IN1-IDARTNR    TO UT4-IDARTNR                               
038119         MOVE IN1-KDPRODSL   TO UT4-KDPRODSL                              
038120         MOVE IN1-IDKONTO    TO UT4-IDKONTO                               
038121         INSPECT UT4-IDKONTO REPLACING LEADING ZERO BY SPACE              
038122         MOVE IN1-IDKST      TO UT4-IDKST                                 
038123         MOVE IN1-IDANALYS   TO UT4-IDANALYS                              
038124         MOVE IN1-SUBEL      TO UT4-SUBEL                                 
038125         MOVE IN1-TECKEN     TO UT4-TECKEN                                
038126         MOVE IN1-IDPRCTR    TO UT4-IDPRCTR                               
038127         MOVE IN1-KDPOST     TO UT4-KDPOST                                
038128         IF IN1-EOF = 'N'                                                 
038129           PERFORM S02-WRITE-LISTA4                                       
038130         END-IF                                                           
038131       END-IF                                                             
038140     END-IF                                                               
038200     .                                                                    
041300 Z-AVSLUTA SECTION.                                                       
041402     CLOSE W5107Q                                                         
041403           W5107R                                                         
041404           W5107S                                                         
041405           W5107T                                                         
041406           W5107QA                                                        
041407           W5107RA                                                        
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800 S01-READ-INFIL SECTION.                                                  
041902     READ W5107Q INTO IN-AREA                                             
042000        AT END MOVE JA TO IN-EOF                                          
042010        DISPLAY 'READ-FIL1:'IN-EOF                                        
042100     END-READ                                                             
042200     .                                                                    
042300 S01-READ-INFIL1 SECTION.                                                 
042301     READ W5107R INTO IN1-AREA                                            
042302        AT END MOVE JA TO IN1-EOF                                         
042303        DISPLAY 'READ-FIL2:'IN1-EOF                                       
042304     END-READ                                                             
042305     .                                                                    
043000 S02-WRITE-LISTA1 SECTION.                                                
043100                                                                          
044601     WRITE LISTPOST1          FROM UT1-AREA AFTER 1                       
044700     .                                                                    
044800                                                                          
044900 S02-WRITE-LISTA2 SECTION.                                                
045000                                                                          
045100     WRITE LISTPOST2          FROM UT2-AREA AFTER 1                       
045200     .                                                                    
045300                                                                          
045400 S02-WRITE-LISTA3 SECTION.                                                
045500                                                                          
045600     WRITE LISTPOST3          FROM UT3-AREA AFTER 1                       
045700     .                                                                    
045800                                                                          
045900 S02-WRITE-LISTA4 SECTION.                                                
046000                                                                          
046100     WRITE LISTPOST4          FROM UT4-AREA AFTER 1                       
046200     .                                                                    
046300                                                                          
