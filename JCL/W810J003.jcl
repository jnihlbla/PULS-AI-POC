//W810J003 JOB (640W8100100W810JACC,W100),'RTN W810D1',                         
//             CLASS=V                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
// EXEC WEZTP,MACLIB1=W.PROD.EPLUSCPY                                           
//EZTP.STEPLIB DD  DSN=W.QASE.LOAD,DISP=SHR                                     
PARM DEBUG (STATE)                                                              
*----------------------------------------------------------------------*        
*                                                                      *        
*  SYSTEM     :   MATCHNING MELLAN LDC-FIL(SDC) OCH KEMIFILEN                   
*                 SKAPAR ETT MAIL ÖVER ARTIKLAR SOM SKALL REVIDERAS             
*                 ETT MAIL SKAPAS MED ALLA LDC:ER OCH SDC:ER                    
*----------------------------------------------------------------------*        
FILE INFIL01                                                                    
%WXTR2D 'KEMI-'                                                                 
*                                                                               
FILE INFIL02                                                                    
%WSDCPART 'IN2-'                                                                
*                                                                               
FILE INFIL03                                                                    
%WCDCPART 'IN3-'                                                                
*                                                                               
FILE TEMP01 FB (650 0)                                                          
%WSDCPART 'TEMP-'                                                               
TEMP-INLEV-DATUM  +638 6 N                                                      
                                                                                
FILE UTFIL01 FB (100 0)                                                         
UTPOST 1 100 A                                                                  
*                                                                               
*********************************************                                   
** WORKING STORAGE                         **                                   
*********************************************                                   
*                                                                               
 W-FIFO          W  6  N                                                        
 W-VECKA         W  6  N                                                        
 W-INLEV-AAMMDD  W  6  N                                                        
 W-DAGENS-AAVV   W  5  N                                                        
 W-DATUM-FOM     W  6  N                                                        
 W-KEMI-KDFGPRIO W  3  N                                                        
 W-IDARTNR       W  9  N                                                        
                                                                                
 W-AAVVD-FOM             W  5  N                                                
 W-AAVV-FOM W-AAVVD-FOM     4  N                                                
 W-AAVVDAG  W-AAVVD-FOM  +4 1  N                                                
                                                                                
 W-INLEV-AAVVD                 W  5  N                                          
 W-INLEV-AAVV W-INLEV-AAVVD       4  N                                          
 W-INLEV-AAVVDAG W-INLEV-AAVVD +4 1  N                                          
                                                                                
 DAGENS-AAVV  W              4  N                                               
 DAGENS-AAR   DAGENS-AAVV    2  N                                               
 DAGENS-VECKA DAGENS-AAVV +2 2  N                                               
                                                                                
 W-DAGENS-DATUM                   W  6  N                                       
 W-DAGENS-AA     W-DAGENS-DATUM      2  N                                       
 W-DAGENS-AAMM   W-DAGENS-DATUM   +2 2  N                                       
 W-DAGENS-AAMMDD W-DAGENS-DATUM   +4 2  N                                       
                                                                                
 W-RUBRIK              W 100 A                                                  
 RUB-0    W-RUBRIK    +1  2 A VALUE 'DC'                                        
 FILLER00 W-RUBRIK    +3  1 A VALUE ';'                                         
 RUB-1    W-RUBRIK    +4  7 A VALUE 'PARTNO '                                   
 FILLER01 W-RUBRIK   +11  1 A VALUE ';'                                         
 RUB-2    W-RUBRIK   +12 11 A VALUE 'DESCRIPTION'                               
 FILLER02 W-RUBRIK   +23  1 A VALUE ';'                                         
 RUB-3    W-RUBRIK   +24  3 A VALUE 'QTY'                                       
 FILLER03 W-RUBRIK   +27  1 A VALUE ';'                                         
 RUB-4    W-RUBRIK   +28  4 A VALUE 'AREA'                                      
 FILLER04 W-RUBRIK   +32  1 A VALUE ';'                                         
 RUB-5    W-RUBRIK   +33  5 A VALUE 'AISLE'                                     
 FILLER05 W-RUBRIK   +38  1 A VALUE ';'                                         
 RUB-6    W-RUBRIK   +39  8 A VALUE 'LOCATION'                                  
 FILLER06 W-RUBRIK   +47  1 A VALUE ';'                                         
 RUB-7    W-RUBRIK   +48 19 A VALUE 'DURABILITY IN WEEKS'                       
 FILLER07 W-RUBRIK   +67  1 A VALUE ';'                                         
 *RUB-8    W-RUBRIK   +68 11 A VALUE 'INLEV DATUM'                              
 *FILLER08 W-RUBRIK   +79  1 A VALUE ';'                                        
 *RUB-9    W-RUBRIK   +80  9 A VALUE 'FIFO AAVV'                                
 *FILLER09 W-RUBRIK   +89  1 A VALUE ';'                                        
 *RUB-10   W-RUBRIK   +90 10 A VALUE 'INLEV AAVV'                               
 *FILLER10 W-RUBRIK   +100 1 A VALUE ';'                                        
                                                                                
 W-UT-RAD                       W  100 A                                        
 W-UT-IDDC          W-UT-RAD     +1  2 A                                        
 W-UT-FILLER00      W-UT-RAD     +3  1 A VALUE ';'                              
 W-UT-IDARTNR       W-UT-RAD     +4  9 N                                        
 W-UT-FILLER01      W-UT-RAD    +13  1 A VALUE ';'                              
 W-UT-BEART-ENG     W-UT-RAD    +14 25 A                                        
 W-UT-FILLER02      W-UT-RAD    +39  1 A VALUE ';'                              
 W-UT-KVLS          W-UT-RAD    +40  8 N                                        
 W-UT-FILLER03      W-UT-RAD    +48  1 A VALUE ';'                              
 W-UT-ADLAGOMR      W-UT-RAD    +49  4 N                                        
 W-UT-FILLER04      W-UT-RAD    +53  1 A VALUE ';'                              
 W-UT-ADGANG        W-UT-RAD    +54  5 N                                        
 W-UT-FILLER05      W-UT-RAD    +59  1 A VALUE ';'                              
 W-UT-ADPLATS       W-UT-RAD    +60  5 N                                        
 W-UT-FILLER06      W-UT-RAD    +65  1 A VALUE ';'                              
 W-UT-KEMI-KDFGPRIO W-UT-RAD    +66  5 N                                        
 *W-UT-FILLER07      W-UT-RAD    +71  1 A VALUE ';'                             
 *W-UT-AAMMDD        W-UT-RAD    +72  6 N                                       
 *W-UT-FILLER08      W-UT-RAD    +78  1 A VALUE ';'                             
 *W-UT-AAVV-FOM      W-UT-RAD    +79  4 N                                       
 *W-UT-FILLER09      W-UT-RAD    +83  1 A VALUE ';'                             
 *W-UT-INLEV-AAVV    W-UT-RAD    +84  4 N                                       
 *W-UT-FILLER10      W-UT-RAD    +88  1 A VALUE ';'                             
*                                                                               
*--------------------------*                                                    
* AREOR FÖR SUBPROGRAM     *                                                    
*--------------------------*                                                    
* ----------------                                                              
* ----------------                                                              
PROGRAM-NAMN      W   8   A   VALUE 'KEMIREV'                                   
DATUMKORT-ID      W   6   A   VALUE 'WDATUM'                                    
DATKORT           W   8   A   VALUE 'DATKORT '                                  
WDATKONV          W   8   A   VALUE 'WDATKONV'                                  
WORKDAY           W   8   A   VALUE 'WORKDAY '                                  
%WDATKORT 'DATKORT-'                                                            
%WDATAREA                                                                       
%WORKAREA                                                                       
* ----------------                                                              
***********************************************************                     
* MATCHAR EXTRAKTFIL-CDC MED EXTRAKTFIL-LDC, HÄMTA INLEV DATUM                  
***********************************************************                     
JOB INPUT (INFIL03 KEY(IN3-IDARTNR) INFIL02 KEY(IN2-IDARTNR))                   
   IF MATCHED                                                                   
     TEMP-WSDCPART EQ IN2-WSDCPART                                              
     TEMP-INLEV-DATUM EQ IN3-TIAVIDAT-SEN                                       
     PUT TEMP01                                                                 
   ELSE                                                                         
     IF INFIL02                                                                 
       DISPLAY ' ***** ART SAKNAS PÅ CDCFILEN *** ' IN2-IDARTNR                 
     END-IF                                                                     
   END-IF                                                                       
***********************************************************                     
* MATCHAR TEMP-EXTRAKTFIL MED LAGERBAND                        *                
***********************************************************                     
JOB INPUT (INFIL01 KEY(KEMI-IDARTNR) TEMP01  KEY(TEMP-IDARTNR)) +               
   START A-INIT                                                                 
   IF MATCHED                                                                   
*   IF TEMP-IDDC EQ '1A' '1B' '1C'                               +              
*              '2A' '2B' '2C' '2D' '2E' '2F' '2G' '2H' '2I' '2J' +              
* ALLA DC:N    '3A' '3B' '3C' '3D' '3E'                                         
                                                                                
     IF KEMI-KDFGPRIO > 0 AND TEMP-KVLS GT 0                                    
      IF TEMP-KDERS < 20 AND TEMP-KDERS-UTG = 0                                 
*** KEMI-KDFGPRIO ÄR ANTAL VECKOR, * 5 => ANTAL DAGAR                           
        IF KEMI-KDFGPRIO > 199                                                  
          W-KEMI-KDFGPRIO = 999                                                 
          W-FIFO = W-KEMI-KDFGPRIO * 0.90                                       
        ELSE                                                                    
          IF KEMI-KDFGPRIO < 80                                                 
            W-KEMI-KDFGPRIO = KEMI-KDFGPRIO * 5                                 
            W-FIFO  =  W-KEMI-KDFGPRIO * 0.50                                   
          ELSE                                                                  
            IF KEMI-KDFGPRIO > 80                                               
              W-KEMI-KDFGPRIO = KEMI-KDFGPRIO * 5                               
              W-FIFO  =  W-KEMI-KDFGPRIO * 0.90                                 
            END-IF                                                              
          END-IF                                                                
        END-IF                                                                  
        IF W-IDARTNR NE TEMP-IDARTNR                                            
          PERFORM B-DATUM                                                       
*         DISPLAY 'ART DATUMLÄS ' TEMP-IDARTNR ' DC ' TEMP-IDDC +               
*         ' FOM ' W-AAVV-FOM ' INLEV ' W-INLEV-AAVV                             
          W-IDARTNR EQ TEMP-IDARTNR                                             
        END-IF                                                                  
        IF W-AAVV-FOM > W-INLEV-AAVV AND TEMP-KVLS GT 0                         
          W-UT-IDDC           EQ TEMP-IDDC                                      
          W-UT-IDARTNR        EQ TEMP-IDARTNR                                   
          W-UT-BEART-ENG      EQ TEMP-BEART-ENG                                 
          W-UT-KVLS           EQ TEMP-KVLS                                      
          W-UT-ADLAGOMR       EQ TEMP-ADLAGOMR                                  
          W-UT-ADGANG         EQ TEMP-ADGANG                                    
          W-UT-ADPLATS        EQ TEMP-ADPLATS                                   
          W-UT-KEMI-KDFGPRIO  EQ KEMI-KDFGPRIO                                  
*         W-UT-AAMMDD         EQ TEMP-INLEV-DATUM                               
*         W-UT-AAVV-FOM       EQ W-AAVV-FOM                                     
*         W-UT-INLEV-AAVV     EQ W-INLEV-AAVV                                   
          UTPOST EQ W-UT-RAD                                                    
          PUT UTFIL01                                                           
        END-IF                                                                  
      END-IF                                                                    
     END-IF                                                                     
**  END-IF  ALLA DC:N                                                           
   END-IF                                                                       
A-INIT. PROC                                                                    
  CALL DATKORT  USING (PROGRAM-NAMN +                                           
                       DATUMKORT-ID +                                           
                       DATKORT-DATUMKORT)                                       
                                                                                
  DAGENS-AAR   =  DATKORT-D-AAR                                                 
  DAGENS-VECKA =  DATKORT-D-VECKA                                               
  W-DAGENS-AA      =  DATKORT-D-AAR                                             
  W-DAGENS-AAMM    =  DATKORT-D-MAANAD                                          
  W-DAGENS-AAMMDD  =  DATKORT-D-DAG                                             
                                                                                
  UTPOST EQ W-RUBRIK                                                            
  PUT UTFIL01                                                                   
END-PROC                                                                        
*****************************************                                       
B-DATUM. PROC                                                                   
*****************************************                                       
**********************************                                              
*** HÄMTA AAVV FÖR INLEV DATUM ***                                              
**********************************                                              
                                                                                
  DAT-I-TIDATUM = TEMP-INLEV-DATUM                                              
  DAT-KDDATFORM = 'AAMMDD'                                                      
  CALL WDATKONV USING (DAT-KDDATFORM, +                                         
                       DAT-I-TIDATUM, +                                         
                       DAT-O-TIDATUM, +                                         
                       DAT-KDSVAR)                                              
 W-INLEV-AAVVD  = DAT-TIAAVVD                                                   
                                                                                
**********************************************************                      
*** HÄMTA VILKET DATUM SOM DET ÄR X(FIFO) DAGAR TILLBAKA *                      
**********************************************************                      
                                                                                
  WORK-KDCALL = 003                                                             
  WORK-IDDC   = '11'                                                            
  WORK-TIAAMMDD-TOM = W-DAGENS-DATUM                                            
  WORK-KVWORKD      = W-FIFO                                                    
  CALL WORKDAY USING (WORK-KDCALL        +                                      
                      WORK-DATE-AREA     +                                      
                      WORK-KDSVAR)                                              
  IF WORK-KDSVAR = ' '                                                          
    W-DATUM-FOM  EQ WORK-TIAAMMDD-FOM                                           
  END-IF                                                                        
                                                                                
                                                                                
****************************************************                            
*** HÄMTAR AAVV FÖR W-DATUM-FOM                *****                            
****************************************************                            
        DAT-I-TIDATUM = W-DATUM-FOM                                             
        DAT-KDDATFORM = 'AAMMDD'                                                
        CALL WDATKONV USING (DAT-KDDATFORM, +                                   
                             DAT-I-TIDATUM, +                                   
                             DAT-O-TIDATUM, +                                   
                             DAT-KDSVAR)                                        
        W-AAVVD-FOM = DAT-TIAAVVD                                               
                                                                                
END-PROC                                                                        
*********************************************************                       
//INFIL01   DD   DSN=W111.W100V1.W11184(+0),DISP=SHR                            
//INFIL02   DD   DSN=WXTR.PARTINFO.WSDCWEEK(+0),DISP=SHR                        
//INFIL03   DD   DSN=WXTR.PARTINFO.WCDCWEEK(+0),DISP=SHR                        
//*                                                                             
//EZTP.TEMP01   DD  DSN=&&TEMP,DISP=(NEW,DELETE,DELETE),                        
//************ TEMPORÄR FIL                                                     
//             DATACLAS=PSEB,MGMTCLAS=NOBACKUP                                  
//*                                                                             
//UTFIL01   DD DSN=&&WSDCWEEK,DISP=(NEW,PASS,DELETE),                           
//          DATACLAS=PSEN,MGMTCLAS=BACKUPC                                      
//OSDAT    DD  DSN=&INDRTE..DATUM(+0),DISP=SHR              -- DATKORT          
//DATIN    DD  DSN=F1ST00.CONSTANT(NORMAL),DISP=SHR         -- DATKORT          
//DATUT    DD  SYSOUT=*                                     -- DATKORT          
//*                                                                             
//* CONVERT FB FILE TO VB FILE                                                  
//SORT     EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SORTIN   DD  DSN=&&WSDCWEEK,DISP=(OLD,DELETE,DELETE)                          
//VBOUT    DD  DSN=WXTR.KEMIREV3.WSDCWEEK(+1),                                  
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEB,MGMTCLAS=NOBACKUP                                  
//SYSIN    DD  *                                                                
 OPTION COPY                                                                    
 OUTFIL FNAMES=VBOUT,FTOV                                                       
//*                                                                             
//* SEND THE LIST VIA EMAIL FROM D&P                                            
//EMPTY1 EXEC WEMPTST,DSIN=WXTR.KEMIREV3.WSDCWEEK(+1)                           
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=WXTR.KEMIREV3.WSDCWEEK(+1)                                
//SYSIN           DD *                                                          
WSDCWEEK                                                                        
WSDCWEEK                                                                        
//    ENDIF                                                                     
//*                                                                             
