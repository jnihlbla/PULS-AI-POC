000010*** EDIT ALLOWED                                                          
000020*    COPYTEXT FRÅN INKÖP, NYTT GRÄNSSNITT FÖR UPPDATERING                 
000030*                         AV LEVERANTÖRDATA                               
000040*    KOPIA FRÅN PI.TEST.COBOLCPY(A3148100)                                
000100*    ADDRESS RECORD                                                       
000200 01  A31481.                                                              
000300*                             GSDB-ID                                     
000400     03  SITEID              PIC X(5).                                    
000500*                             DATE OF CHANGE YYYYMMDD                     
000600     03  DATUPD              PIC X(8).                                    
000700*                             SITE STATUS                                 
000800     03  CDSITESTAT          PIC X(2).                                    
000900*                             NAME OF CUSTOMER/VENDOR                     
001000     03  SITENAME            PIC X(60).                                   
001100*                             STREET AND HOUSENUMBER                      
001200*                             AS PART OF ADDRESS                          
001300     03  SITEADR             PIC X(35).                                   
001400*                             MAILING ADDRESS LINE 1                      
001500     03  SITEADR1            PIC X(35).                                   
001600*                             MAILING ADDRESS LINE 2                      
001700     03  SITEADR2            PIC X(42).                                   
001800*                             POSTAL CODE/ZIP CODE + CITY OR              
001900*                             CITY + COUNTRY (GB)         OR              
002000*                             CITY + STATE + POSTAL CODE                  
002100     03  SITEPADR            PIC X(46).                                   
002200*                             COUNTRY NAME                                
002300     03  SITECTRY            PIC X(20).                                   
002400*                             COUNTRY CODE                                
002500     03  CDCTRY              PIC X(2).                                    
002600*                             POSTAL CODE / ZIP CODE                      
002700     03  SITEPC              PIC X(9).                                    
002800*                             CITY, NAME OF POSTOFFICE CITY               
002900     03  SITECITY            PIC X(32).                                   
003000*                             SEARCHTERM                                  
003100     03  SITESHNAME          PIC X(15).                                   
003200*                             TELEPHONE                                   
003300     03  PHONE               PIC X(20).                                   
003400*                             FAX                                         
003500     03  FAX                 PIC X(20).                                   
003600*                             STATE, COUNTRY, PROVINCE                    
003700     03  STATE               PIC X(2).                                    
003800*                             LANGUAGE CODES IN USE                       
003900     03  CDLANGUAGE          PIC X(2).                                    
004000*                             VAT REGISTRATION NUMBER                     
004100     03  VAT                 PIC X(20).                                   
004200*                             SP-BUILDING                                 
004300     03  BUILDING            PIC X(32).                                   
004400*                             SUPPLIERS ROLES                             
004500     03  ROLE                PIC X(10).                                   
004600*                             VENDOR - AUTOMOTIVE PRODUCT                 
004700     03  AP                  PIC X(1).                                    
004800*                             VENDOR - NON AUTOMOTIVE PRODUCT             
004900     03  NAP                 PIC X(1).                                    
005000*                             PAY-TERMS                                   
005100     03  CDPAYT              PIC X(2).                                    
005200*                             GROUP/NON-GROUP                             
005300     03  GROUP-CD            PIC X(1).                                    
005400*                             CURRENCY CODE ISO                           
005500     03  CDCURRENCY          PIC X(3).                                    
005400*                             TYPE OF UPDATE (N,U,D)                      
005500     03  CDUPDAT             PIC X(1).                                    
005400*                                                                         
005600*** END OF VILMAII-COPY LENGTH= 426 OLD LENGTH=                           
