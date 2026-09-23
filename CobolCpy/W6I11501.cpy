000100 01  MID-W6I11501.                                                        
000200*                                 MID-COPYTEXT FÖR W60115                 
000300     03 MID-NYCKLAR-GRP.                                                  
000400*                                 BILDGRUPP 6111-6119                     
000500        05 MID-IDLEVNR-IN    PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700        05 MID-IDLEVNR-UT    PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900        05 MID-IDFS-IN       PIC X(8).                                    
001000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001100        05 MID-IDFS-UT       PIC X(8).                                    
001200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001300        05 MID-TIAVIDAT-IN   PIC X(6).                                    
001400*                                 AVISERINGSDATUM (YYMMDD)                
001500        05 MID-TIAVIDAT-UT   PIC X(6).                                    
001600*                                 AVISERINGSDATUM (YYMMDD)                
001700        05 MID-ADINLOMR-PRT-IN                                            
001800                             PIC X(4).                                    
001900*                                 PRINTERPLACERING                        
002000        05 MID-ADINLOMR-PRT-UT                                            
002100                             PIC X(4).                                    
002200*                                 PRINTERPLACERING                        
002300        05 MID-IDDC-IN       PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500        05 MID-IDDC-UT       PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700        05 MID-KDRT-IN       PIC X(2).                                    
002800*                                 REDOVISNINGSTYP                         
002900        05 MID-KDRT-UT       PIC X(2).                                    
003000*                                 REDOVISNINGSTYP                         
003100        05 MID-IDLBBET-IN    PIC X(12).                                   
003200*                                 LASTBÄRARBETECKNING                     
003300        05 MID-IDLBBET-UT    PIC X(12).                                   
003400*                                 LASTBÄRARBETECKNING                     
003500        05 MID-FLKLIVIS-IN   PIC X.                                       
003600*                                 JA/NEJ-FLAGGA                           
003700        05 MID-FLKLIVIS-UT   PIC X.                                       
003800*                                 JA/NEJ-FLAGGA                           
003900        05 MID-IDLOPNRM-IN   PIC X(8).                                    
004000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004100*                                 (0VVDLLLLK)                             
004200        05 MID-IDLOPNRM-UT   PIC X(8).                                    
004300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004400*                                 (0VVDLLLLK)                             
004500     03 MID-IDLEVNR-ENTER    PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700     03 MID-IDLEVNR-NEXT     PIC X(5).                                    
004800*                                 LEVERANTÖRNUMMER                        
004900     03 MID-IDFS-ENTER       PIC X(8).                                    
005000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005100     03 MID-IDFS-NEXT        PIC X(8).                                    
005200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005300     03 MID-TIAVIDAT-ENTER   PIC 9(6).                                    
005400*                                 AVISERINGSDATUM (YYMMDD)                
005500     03 MID-TIAVIDAT-NEXT    PIC 9(6).                                    
005600*                                 AVISERINGSDATUM (YYMMDD)                
005700     03 MID-IDLBBET-ENTER    PIC X(12).                                   
005800*                                 LASTBÄRARBETECKNING                     
005900     03 MID-IDLBBET-NEXT     PIC X(12).                                   
006000*                                 LASTBÄRARBETECKNING                     
006100     03 MID-OMSTART-INDX     PIC 9(2).                                    
006200     03 MID-KDCMDVAL-TAB.                                                 
006300*                                 TABELL MED KDCMDVAL                     
006400        05 MID-KDCMDVAL-RAD  OCCURS 10 TIMES                              
006500                             PIC X(3).                                    
006600*                                 GENERELL KOMMANDOKOD                    
006700     03 MID-IDLEVNR-RAD      OCCURS 10 TIMES                              
006800                             PIC X(5).                                    
006900*                                 LEVERANTÖRNUMMER                        
007000     03 MID-IDFS-RAD         OCCURS 10 TIMES                              
007100                             PIC X(8).                                    
007200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
007300     03 MID-TIAVIDAT-RAD     OCCURS 10 TIMES                              
007400                             PIC X(6).                                    
007500*                                 AVISERINGSDATUM (YYMMDD)                
007600     03 MID-INPUT.                                                        
007700*                                 INDATA FÖR UPPDATERING                  
007800        05 MID-IDLBBET-UPD   PIC X(12).                                   
007900*                                 LASTBÄRARBETECKNING                     
008000        05 MID-FLKLAR-UPD    PIC X.                                       
008100*                                 AVSLUTNINGSMARKERING                    
008200        05 MID-ADINLOMR-LPL-UPD                                           
008300                             PIC X(4).                                    
008400*                                 LOSSNINGSPLATS                          
008500     03 MID-KVUTSKR-AR       PIC 9(3).                                    
008600*** END OF VILMAII-COPY LENGTH= 400 BYTES                                 
