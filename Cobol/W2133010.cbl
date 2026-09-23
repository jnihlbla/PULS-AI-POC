000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2133010.                                    
000300 AUTHOR.                     IDK,GÖTEBORG.                                
000400 DATE-WRITTEN.               NOVEMBER 1978.                               
000500     SKIP3                                                                
000600*REMARKS.                                                                 
000700*    FUNKTION.                                                            
000800*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W2133000.                      
000900*        SUBPROGRAMMET SKÖTER  SAMTLIGA IMS-CALL                          
001000*        MOT HÄNDELSEREGISTRET, LOGISKT (WLXXBN).                         
001100     SKIP3                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP3                                                                
001400 DATA DIVISION.                                                           
001500     EJECT                                                                
001600 WORKING-STORAGE SECTION.                                                 
001610                                                                          
001700*    -- CHECKED BY WY2000                                                 
002100 77      JA              PIC X       VALUE 'J'.                           
002200 77      NEJ             PIC X       VALUE 'N'.                           
002300     SKIP3                                                                
002400 01      KONSTANTER.                                                      
002500   03    LAES-ROT        PIC S9(3)   VALUE +101      COMP-3.              
002600   03    LAES-DELETE-SEGM3                                                
002700                         PIC S9(3)   VALUE +102      COMP-3.              
002800     SKIP3                                                                
002900 01      W-WDG3KEY-X.                                                     
003000   03    W-WDG3KEY1-4    PIC X(4).                                        
003100   03    W-WDG3KEY5-30   PIC X(26)   VALUE LOW-VALUE.                     
003200     SKIP3                                                                
003300 01      DYNAMISKA-SUBPROGRAM.                                            
003400   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
003500   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
003600     EJECT                                                                
003700*----------------------------------------- ARBETSAREOR                    
003800*                                          TILL IMS-SEKTIONERNA           
003900 01      IMS-WS.                                                          
004000   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
004100     SKIP3                                                                
004200*---------------------------------------- STATUSKOD FRÅN IMS              
004300   03    STATUS-WS       PIC XX.                                          
004400     88  SEGMENT-FINNS               VALUE '  '.                          
004500     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
004600     SKIP3                                                                
004700   03    SSA1            PIC X(60).                                       
004800   03    SSA2            PIC X(60).                                       
004900     SKIP3                                                                
005000   03    GODK-STATUSKODER.                                                
005100     05  GODK-STATUS     OCCURS 10                                        
005200                         INDEXED BY STATUS-IX                             
005300                         PIC X(2).                                        
005400     SKIP3                                                                
005500*01      -COPY W0003                                                      
005700     SKIP3                                                                
005800 01      DLI-IO-AREA     PIC X(30)   VALUE SPACE.                         
005900     SKIP2                                                                
006000*01  WLXXBN11  -COPY WDGX2222C0 -PRE WDG303- -RED DLI-IO-AREA             
006200     EJECT                                                                
006300 LINKAGE SECTION.                                                         
006400 01  LINK-AREA               PIC X(30).                                   
006500*01      HLINK -COPY W213L301 -PRE LINK-   -RED LINK-AREA                 
006700     EJECT                                                                
006800*01      -COPY W0008     -PRE BN-                                         
007000      05 FILLER          PIC X.                                           
007100     EJECT                                                                
007200 PROCEDURE DIVISION USING LINK-AREA BN-PCB.                               
007300     ENTRY 'DLITCBL' USING LINK-AREA BN-PCB.                              
007400                                                                          
007500     EVALUATE LINK-KDCALL                                                 
007600       WHEN LAES-ROT                                                      
007700         PERFORM A-LAES-ROT                                               
007800                                                                          
007900       WHEN LAES-DELETE-SEGM3                                             
008000         PERFORM B-LAES-DELETE-SEGM3                                      
008100                                                                          
008200       WHEN OTHER                                                         
008300         MOVE NEJ TO LINK-FLJANEJ-ANROP                                   
008400     END-EVALUATE                                                         
008500                                                                          
008600     MOVE ZERO TO RETURN-CODE                                             
008700     GOBACK                                                               
008800     .                                                                    
008900     EJECT                                                                
009000 A-LAES-ROT SECTION.                                                      
009100                                                                          
009200     MOVE NEJ         TO LINK-FLJANEJ-ANROP                               
009300     MOVE LOW-VALUE   TO LINK-IO-AREA                                     
009400     MOVE LINK-IDHTYP TO W-WDG3KEY1-4                                     
009500                                                                          
009600     PERFORM IMS-GET-WLXXBN01                                             
009700     IF SEGMENT-FINNS                                                     
009800       MOVE JA TO LINK-FLJANEJ-ANROP                                      
009900     END-IF                                                               
010000     .                                                                    
010100     EJECT                                                                
010200 B-LAES-DELETE-SEGM3 SECTION.                                             
010300                                                                          
010500     MOVE NEJ       TO LINK-FLJANEJ-ANROP                                 
010510     MOVE LOW-VALUE TO LINK-IO-AREA                                       
010600                                                                          
010700     PERFORM IMS-GET-WLXXBN11                                             
010800                                                                          
010900     IF SEGMENT-FINNS                                                     
011000       MOVE JA                   TO LINK-FLJANEJ-ANROP                    
011100       MOVE WDG303-2222-IDARTNR  TO LINK-IDARTNR                          
011200       MOVE WDG303-2222-IDLEVNR  TO LINK-IDLEVNR                          
011300       MOVE WDG303-2222-IDSYSTEM TO LINK-IDSYSTEM                         
011400       PERFORM IMS-DELETE-WLXXBN11                                        
011500     END-IF                                                               
011510     .                                                                    
011600     EJECT                                                                
014500******************************************************************        
014600*    I M S   S E C T I O N E R                                            
014700******************************************************************        
014727 IMS-GET-WLXXBN01 SECTION.                                                
014728                                                                          
014729     STRING 'WLXXBN01(WDG3KEY  =' W-WDG3KEY-X ')'                         
014730            DELIMITED BY SIZE INTO SSA1                                   
014731     MOVE '  GE' TO GODK-STATUSKODER                                      
014732     CALL CBLTDLI USING GU BN-PCB DLI-IO-AREA SSA1                        
014733     MOVE BN-STATUS-CODE TO STATUS-WS                                     
014734     PERFORM IMS-STATUSKONTROLL                                           
014735     .                                                                    
014736     SKIP3                                                                
014737 IMS-GET-WLXXBN11 SECTION.                                                
014738                                                                          
014739     MOVE 'WLXXBN11 ' TO SSA2                                             
014740     MOVE '  GE' TO GODK-STATUSKODER                                      
014741     CALL CBLTDLI USING GHNP BN-PCB DLI-IO-AREA SSA2                      
014742     MOVE BN-STATUS-CODE TO STATUS-WS                                     
014743     PERFORM IMS-STATUSKONTROLL                                           
014744     .                                                                    
014745     EJECT                                                                
014746 IMS-DELETE-WLXXBN11 SECTION.                                             
014747                                                                          
014748     MOVE '  '   TO GODK-STATUSKODER                                      
014749     CALL CBLTDLI USING DLET BN-PCB DLI-IO-AREA                           
014750     MOVE BN-STATUS-CODE TO STATUS-WS                                     
014751     PERFORM IMS-STATUSKONTROLL                                           
014752     .                                                                    
014753     SKIP3                                                                
014754 IMS-STATUSKONTROLL SECTION.                                              
014761                                                                          
014762     SET STATUS-IX TO 1                                                   
014763     SEARCH GODK-STATUS                                                   
014764       AT END                                                             
014768         CALL FELLOG                                                      
014769       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
014770         CONTINUE                                                         
014771     END-SEARCH                                                           
014780     .                                                                    
