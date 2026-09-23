000100 01  MOD-W2O36601.                                                        
000200*                                 COPYTEXT FOR MOD W2O36601               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 SCREEN NUMBER                           
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS ERROR MESSAGE                       
000700     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 MOD-IDUSER           PIC X(8).                                    
001300*                                 USER SECURITY-IDENTITY                  
001400     03 MOD-TIUPPDAT         PIC Z(6).                                    
001500*                                 UPDATING DATE     (YYMMDD)              
001600     03 MOD-KVVECKOR-LSALES-UP-ATTR                                       
001700                             PIC X(2).                                    
001800     03 MOD-KVVECKOR-LSALES-UP                                            
001900                             PIC X(3).                                    
002000*                                 NN.OF WEEKS SINCE LAST SALES            
002100     03 MOD-KVVECKOR-PUBV-UP-ATTR                                         
002200                             PIC X(2).                                    
002300     03 MOD-KVVECKOR-PUBV-UP PIC X(3).                                    
002400*                                 NO. OF WEEKS SINCE PUBL.WEEK            
002500     03 MOD-PRARTSTD-UP-ATTR PIC X(2).                                    
002600     03 MOD-PRARTSTD-UP      PIC X(7).                                    
002700*                                 STANDARD PRICE                          
002800     03 MOD-VLARTNTO-UP-ATTR PIC X(2).                                    
002900     03 MOD-VLARTNTO-UP      PIC X(8).                                    
003000*                                 PART NET VOLUME    (CM3)                
003100     03 MOD-KDPRODSL-UP-ATTR PIC X(2).                                    
003200     03 MOD-KDPRODSL-UP      PIC X(2).                                    
003300*                                 PRODUCT GROUP                           
003400     03 MOD-AREA-UP          OCCURS 10 TIMES.                             
003500*                                 OCCURS CLAUSE FOR W2036601 COPY         
003600*                                 TEXT                                    
003700        05 MOD-ADLAGOMR-UP-ATTR                                           
003800                             PIC X(2).                                    
003900        05 MOD-ADLAGOMR-UP   PIC X(2).                                    
004000*                                 AREA                                    
004100     03 MOD-DATA-LINES       OCCURS 13 TIMES.                             
004200*                                 OCCURS CLAUSE FOR W2036601 COPY         
004300*                                 TEXT                                    
004400        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
004500        05 MOD-KDCMDVAL      PIC X.                                       
004600*                                 GENERAL COMMAND-CODE                    
004700        05 MOD-KVVECKOR-LSALES                                            
004800                             PIC Z(2)9.                                   
004900*                                 NN.OF WEEKS SINCE LAST SALES            
005000        05 MOD-KVVECKOR-PUBV PIC Z(2)9.                                   
005100*                                 NO. OF WEEKS SINCE PUBL.WEEK            
005200        05 MOD-PRARTSTD      PIC Z(6)9.                                   
005300*                                 STANDARD PRICE                          
005400        05 MOD-VLARTNTO      PIC Z(7)9.                                   
005500*                                 PART NET VOLUME    (CM3)                
005600        05 MOD-KDPRODSL      PIC Z9.                                      
005700*                                 PRODUCT GROUP                           
005800        05 MOD-ADLAGOMR      OCCURS 10 TIMES                              
005900                             PIC Z9.                                      
006000*                                 AREA                                    
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATION MESSAGE                     
006300*** END OF VILMAII-COPY LENGTH= 790 BYTES                                 
