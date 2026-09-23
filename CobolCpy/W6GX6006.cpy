000100 01  6006-W6GX6006.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 PLACERINGSOMRÅDEN                       
000400*                                 FYSISK NYCKEL                           
000500*                                 W6GXKEY =                               
000600*                                 ADINLOMR + LOW-VALUE                    
000700*                                 SÖKFÄLT:                                
000800*                                 ADIMLOMB = ADINLOMR-BO                  
000900*                                 ADIMLOML = ADINLOMR-LPL                 
001000*                                 ADIMLOMP = ADINLOMR-PAR                 
001100*                                 KDINLOMR , KDINLUPF                     
001200     03 6006-ADINLOMR        PIC X(4).                                    
001300*                                 INLEVERANSOMRÅDE                        
001400*                                 RECEIVING AREA                          
001500     03 6006-LOW-VALUE       PIC X.                                       
001600     03 6006-ADGANG-FOM      PIC S9(3)           COMP-3.                  
001700*                                 GÅNG FRÅN OCH MED                       
001800*                                 AISLE ADDRESS FROM                      
001900     03 6006-ADGANG-TOM      PIC S9(3)           COMP-3.                  
002000*                                 GÅNG TILL OCH MED                       
002100*                                 AISLE ADDRESS TO                        
002200     03 6006-ADINLOMR-BO     PIC X(4).                                    
002300*                                 BUFFERTOMRÅDE                           
002400*                                 BUFFER AREA                             
002500     03 6006-ADINLOMR-LPL    PIC X(4).                                    
002600*                                 LOSSNINGSPLATS                          
002700*                                 UNLOADING AREA                          
002800     03 6006-ADINLOMR-PAR    PIC X(4).                                    
002900*                                 INLEVERANSOMRÅDE ÖVERORDNAT             
003000*                                 RECEIVING AREA PARENT                   
003100     03 6006-ADINLOMR-PRT    PIC X(4).                                    
003200*                                 PRINTERPLACERING                        
003300*                                 PLACE OF A PRINTER                      
003400     03 6006-ADPLATS-FOM     PIC S9(5)           COMP-3.                  
003500*                                 LAGERPLATS FRÅN OCH MED                 
003600*                                 LOCATION ADDRESS FROM                   
003700     03 6006-ADPLATS-TOM     PIC S9(5)           COMP-3.                  
003800*                                 LAGERPLATS TILL OCH MED                 
003900*                                 LOCATION ADDRESS TO                     
004000     03 6006-FLKVARED        PIC X.                                       
004100*                                 REDUCERAD KONTROLL FLAGGA               
004200*                                 REDUCED CONTROL FLAG                    
004300     03 6006-FLLOLL          PIC X.                                       
004400*                                 SKAPA LOSSNINGSLISTEFLAGGA              
004500*                                 CREATE UNLOADINGLIST FLAG               
004600     03 6006-IDLEVNR         PIC X(5).                                    
004700*                                 LEVERANTÖRNUMMER                        
004800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004900     03 6006-KDINLOMR        PIC X(3).                                    
005000*                                 TYP AV INLEVERANSOMRÅDE                 
005100*                                 TYPE OF RECEIVING AREA                  
005200     03 6006-KDINLUPF        PIC X(4).                                    
005300*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
005400*                                 FOLLOW-UP STATUS RECEIVING              
005500     03 6006-KDLORAPP        PIC 9.                                       
005600*                                 KOD FÖR R32-RAPPORTERING                
005700*                                 CODE FOR R32-REPORTING                  
005800     03 6006-IDPERSON-ANSV   PIC S9(3)           COMP-3.                  
005900*                                 PERSONKOD                               
006000*                                 STAFF CODE                              
006100     03 6006-IDPERSON-FORP   PIC S9(3)           COMP-3.                  
006200*                                 PERSONKOD                               
006300*                                 STAFF CODE                              
006400     03 6006-IDPERSON-KVAL   PIC S9(3)           COMP-3.                  
006500*                                 PERSONKOD                               
006600*                                 STAFF CODE                              
006700     03 6006-FLCDOMR         PIC X.                                       
006800*                                 FLAGGA CROSS DOCKING OMRÅDE             
006900*                                 FLAG CROSS DOCKING AREA                 
007000     03 6006-FLKNTRGK        PIC X.                                       
007100*                                 OMPLACERING GODK. KONTROLL J/N          
007200*                                 MOVE INVOICE CHECKED-UP J/N             
007300     03 6006-KVTID-NORM      PIC 9(4).                                    
007400*                                 NORMAL MÅLTID FÖR GODSPLACERING         
007500*                                 NORMAL AIM TIME FOR AN ADDRESS          
007600     03 6006-KVTID-PRIO      PIC 9(4).                                    
007700*                                 PRIO MÅLTID FÖR GODSPLACERING           
007800*                                 PRIO AIM TIME WORKFLOW/ADDRESS          
007900     03 6006-FLEXCP          PIC X.                                       
008000*                                 ALLMÄN FLAGGA FÖR UNDANTAG              
008100*                                 GENERAL FLAG FOR EXCEPTION              
008200     03 6006-IDAVD-DAG       PIC X(5).                                    
008300*                                 DEN ANSTÄLLDES AVDELNING/DAG            
008400*                                 DEPARTMENT OF EMPLOYED/DAY              
008500     03 6006-IDAVD-NATT      PIC X(5).                                    
008600*                                 DEN ANSTÄLLDES AVDELNING/NATT           
008700*                                 DEPARTMENT OF EMPLOYED/NIGHT            
008800     03 6006-IDGRUPP-DAG     PIC X(2).                                    
008900*                                 DEN ANSTÄLLDES GRUPPID/DAG              
009000*                                 TEAM ID OF EMPLOYED/DAY                 
009100     03 6006-IDGRUPP-NATT    PIC X(2).                                    
009200*                                 DEN ANSTÄLLDES GRUPPID/NATT             
009300*                                 TEAM ID OF EMPLOYED/NIGHT               
009400     03 6006-FILLER          PIC X(10).                                   
009500*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
