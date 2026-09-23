003095 ID DIVISION.                                                             
003096     SKIP2                                                                
003097 PROGRAM-ID.     W9600220.                                                
003098 AUTHOR.         KARIN OLSSON.                                            
003099 DATE-WRITTEN.   92/12/18.                                                
003100                                                                          
003101     REMARKS.                                                             
003102*                                                                         
003103*    FUNKTION:                                                            
003104*        PROGRAM FÖR ATT RETURNERA EN RAD I EN KOMPILERINGSLISTA.         
003105*                                                                         
003106*    RETURKODER:                                                          
003107*        4 OM TILLFÄLLIGT SLUT PÅ DATA                                    
003108*        8 OM SLUT PÅ DATA                                                
003109*       16 OM I/O-ERROR                                                   
003110*                                                                         
003111     SKIP3                                                                
003112 ENVIRONMENT DIVISION.                                                    
003113     SKIP2                                                                
003114 INPUT-OUTPUT SECTION.                                                    
003115     EJECT                                                                
003116 DATA DIVISION.                                                           
003117     SKIP3                                                                
003118 WORKING-STORAGE SECTION.                                                 
003119     SKIP2                                                                
003120                                                                          
003121*    -- CHECKED BY WY2000                                                 
003122 77  IDPGM                       PIC X(8)    VALUE 'W9600220'.            
003130 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003201 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
003203     SKIP2                                                                
003204 01  GENERELLA-SUBPROGRAM.                                                
003205   03  ISPLINK                   PIC X(8)    VALUE 'ISPLINK '.            
003206   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
003207   03  W9600221                  PIC X(8)    VALUE 'W9600221'.            
003208     SKIP2                                                                
003209 77  WDUMMY                      PIC X(8)    VALUE SPACE.                 
003211 77  ISP-VDEFINE                 PIC X(8)    VALUE 'VDEFINE '.            
003212 77  ISP-VDELETE                 PIC X(8)    VALUE 'VDELETE '.            
003214 77  ISP-VPUT                    PIC X(8)    VALUE 'VPUT    '.            
003215 77  ISP-SHARED                  PIC X(8)    VALUE 'SHARED  '.            
003218 77  SETMSG                      PIC X(8)    VALUE 'SETMSG  '.            
003219 77  EMPTY-MSG                   PIC X(8)    VALUE 'LISTB000'.            
003220 77  TABELL                      PIC X(8).                                
003221 77  PACK                        PIC X(8)    VALUE 'PACK    '.            
003222 77  CHAR                        PIC X(8)    VALUE 'CHAR    '.            
003223 77  FIXED                       PIC X(8)    VALUE 'FIXED   '.            
003224 77  VDEFINE-OPT                 PIC X(16)                                
003225                              VALUE '(COPY NOBSCAN)'.                     
003230     SKIP2                                                                
011812 01  NYSECT                      PIC X.                                   
011813 01  N-NYSECT                    PIC X(8)    VALUE 'NYSECT'.              
011814 01  L-NYSECT                    PIC S9(9)   COMP VALUE +1.               
011815     SKIP2                                                                
011816 01  RADLRECL                    PIC 9(3)    COMP-3.                      
011817 01  N-RADLRECL                  PIC X(8)    VALUE 'RADLRECL'.            
011818 01  L-RADLRECL                  PIC S9(9)   COMP VALUE +2.               
011819     SKIP2                                                                
011820 01  TOPRADNR                    PIC 9(5)    COMP-3.                      
011821 01  N-TOPRADNR                  PIC X(8)    VALUE 'TOPRADNR'.            
011822 01  L-TOPRADNR                  PIC S9(9)   COMP VALUE +3.               
011823     SKIP2                                                                
011825 01  MAXRADNR                    PIC 9(5)    COMP-3.                      
011826 01  N-MAXRADNR                  PIC X(8)    VALUE 'MAXRADNR'.            
011827 01  L-MAXRADNR                  PIC S9(9)   COMP VALUE +3.               
011828     SKIP2                                                                
011830 01  RAD                         PIC X(150).                              
011833     SKIP2                                                                
011835 01  TAB-RADNR                   PIC 9(8)    COMP.                        
011836     SKIP2                                                                
011840 01  RKOD                        PIC S9(4)   COMP.                        
011850     EJECT                                                                
011900 LINKAGE SECTION.                                                         
012020     SKIP2                                                                
012100 01  LIST-RAD-ADRESS             PIC S9(8)   COMP SYNC.                   
012200 01  LIST-LAENGD                 PIC 9(8)    COMP.                        
012300 01  RADNR                       PIC 9(8)    COMP.                        
012310 01  DATA-ADRESS                 PIC S9(8)   COMP SYNC.                   
016200                                                                          
016700     EJECT                                                                
016800 PROCEDURE DIVISION USING LIST-RAD-ADRESS LIST-LAENGD                     
016810                        RADNR DATA-ADRESS.                                
016900     SKIP2                                                                
017000     MOVE ZERO TO RKOD                                                    
017100                                                                          
017116     MOVE 'RADTAB1' TO TABELL                                             
017120                                                                          
017121     CALL ISPLINK USING ISP-VDELETE N-NYSECT                              
017122     CALL ISPLINK USING ISP-VDELETE N-RADLRECL                            
017123     CALL ISPLINK USING ISP-VDELETE N-TOPRADNR                            
017124     CALL ISPLINK USING ISP-VDELETE N-MAXRADNR                            
017125                                                                          
017126     CALL ISPLINK USING ISP-VDEFINE N-NYSECT NYSECT CHAR                  
017127                         L-NYSECT VDEFINE-OPT                             
017128     IF RETURN-CODE > 0                                                   
017129       DISPLAY 'SAKNAR NYSECT'                                            
017130       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017131     END-IF                                                               
017132                                                                          
017133     CALL ISPLINK USING ISP-VDEFINE N-RADLRECL RADLRECL PACK              
017134                         L-RADLRECL VDEFINE-OPT                           
017135     IF RETURN-CODE > 0                                                   
017136       DISPLAY 'SAKNAR RADLRECL'                                          
017137       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017138     END-IF                                                               
017139                                                                          
017140     CALL ISPLINK USING ISP-VDEFINE N-TOPRADNR TOPRADNR PACK              
017141                         L-TOPRADNR VDEFINE-OPT                           
017142     IF RETURN-CODE > 0                                                   
017143       DISPLAY 'SAKNAR TOPRADNR'                                          
017144       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017145     END-IF                                                               
017146                                                                          
017147     CALL ISPLINK USING ISP-VDEFINE N-MAXRADNR MAXRADNR PACK              
017148                         L-MAXRADNR VDEFINE-OPT                           
017149     IF RETURN-CODE > 0                                                   
017150       DISPLAY 'SAKNAR MAX ANTAL RADER'                                   
017151       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017152     END-IF                                                               
017160                                                                          
017170     MOVE RADLRECL TO LIST-LAENGD                                         
017171                                                                          
017176     IF RADNR = 99999999                                                  
017177       MOVE MAXRADNR TO RADNR                                             
017178       MOVE 4 TO RKOD                                                     
017179     ELSE                                                                 
017181       IF NYSECT = 'J'                                                    
017184         IF MAXRADNR = ZERO                                               
017185           CALL ISPLINK USING SETMSG EMPTY-MSG                            
017186           MOVE SPACE TO RAD                                              
017187           CALL W9600221 USING RAD LIST-RAD-ADRESS                        
017190           MOVE 1 TO MAXRADNR                                             
017191         ELSE                                                             
017192           IF RADNR > MAXRADNR                                            
017193             MOVE TOPRADNR TO TAB-RADNR                                   
017194           ELSE                                                           
017195             COMPUTE TAB-RADNR = (TOPRADNR + RADNR) - 1                   
017197           END-IF                                                         
017198           COMPUTE LIST-RAD-ADRESS =                                      
017199                         DATA-ADRESS + 150 * TAB-RADNR                    
017200         END-IF                                                           
017206         MOVE MAXRADNR TO RADNR                                           
017207         MOVE 4 TO RKOD                                                   
017208       ELSE                                                               
017209         IF MAXRADNR = ZERO                                               
017210           CALL ISPLINK USING SETMSG EMPTY-MSG                            
017211           MOVE SPACE TO RAD                                              
017212           CALL W9600221 USING RAD LIST-RAD-ADRESS                        
017213           IF RADNR > 1                                                   
017214             MOVE 8 TO RKOD                                               
017215             MOVE 1 TO RADNR                                              
017216           END-IF                                                         
017217         ELSE                                                             
017218           IF RADNR > MAXRADNR                                            
017219             MOVE MAXRADNR TO RADNR                                       
017220             COMPUTE TAB-RADNR = (TOPRADNR + RADNR) - 1                   
017221             COMPUTE LIST-RAD-ADRESS =                                    
017222                           DATA-ADRESS + 150 * TAB-RADNR                  
017223             MOVE 8 TO RKOD                                               
017230           ELSE                                                           
017411             COMPUTE TAB-RADNR = (TOPRADNR + RADNR) - 1                   
017428             COMPUTE LIST-RAD-ADRESS =                                    
017429                           DATA-ADRESS + 150 * TAB-RADNR                  
017434           END-IF                                                         
017435         END-IF                                                           
017436       END-IF                                                             
017437     END-IF                                                               
017438                                                                          
017439     MOVE 'N' TO NYSECT                                                   
017440     CALL ISPLINK USING ISP-VPUT N-NYSECT ISP-SHARED                      
017441     IF RETURN-CODE > 0                                                   
017442       DISPLAY 'KAN INTE VPUTTA NYSECT'                                   
017443       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017444     END-IF                                                               
017445                                                                          
017480     MOVE RKOD TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
