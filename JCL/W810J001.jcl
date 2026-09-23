//W810J001 JOB (640W8100100W810JACC,W100),'RTN W810D1',                         
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
*  SYSTEM     :   MATCHNING MELLAN VECKOLAGERBAND OCH KEMIFILEN                 
*                 SKAPAR ETT MAIL ÖVER ARTIKLAR SOM SKALL REVIDERAS             
*                 UPPDRAGSGIVARE: ROLF ANDERSSON                                
*                                                                      *        
*----------------------------------------------------------------------*        
FILE INFIL01                                                                    
%WXTR2D 'KEMI-'                                                                 
*                                                                               
FILE INFIL02                                                                    
%WCDCPART                                                                       
*                                                                               
FILE UTFIL01 FB (80 0)                                                          
UTPOST 1 80 A                                                                   
*                                                                               
*********************************************                                   
** WORKING STORAGE                         **                                   
*********************************************                                   
*                                                                               
 W-FIFO          W  3  N                                                        
 W-KEMI-KDFGPRIO W  3  N                                                        
 W-VECKA         W  6  N                                                        
 W-INLEV-AAMMDD  W  6  N                                                        
 W-DAGENS-AAVV   W  5  N                                                        
 W-DAGENS-FOM    W  6  N                                                        
 W-DATUM-FOM     W  6  N                                                        
                                                                                
 W-AAVVD-FOM             W  5  N                                                
 W-AAVV-FOM W-AAVVD-FOM     4  N                                                
 W-AAVVDAG  W-AAVVD-FOM  +4 1  N                                                
                                                                                
 W-INLEV-AAVVD                 W  5  N                                          
 W-INLEV-AAVV W-INLEV-AAVVD       4  N                                          
 W-INLEV-AAVVDAG W-INLEV-AAVVD +4 1  N                                          
                                                                                
 W-DAGENS-DATUM                   W  6  N                                       
 W-DAGENS-AA     W-DAGENS-DATUM      2  N                                       
 W-DAGENS-AAMM   W-DAGENS-DATUM   +2 2  N                                       
 W-DAGENS-AAMMDD W-DAGENS-DATUM   +4 2  N                                       
                                                                                
 DAGENS-AAVV  W              4  N                                               
 DAGENS-AAR   DAGENS-AAVV    2  N                                               
 DAGENS-VECKA DAGENS-AAVV +2 2  N                                               
                                                                                
 W-RUBRIK              W  80 A                                                  
 RUB-1    W-RUBRIK    +1  7 A VALUE 'ARTIKEL'                                   
 FILLER01 W-RUBRIK    +8  1 A VALUE ';'                                         
 RUB-2    W-RUBRIK    +9  9 A VALUE 'BENÄMNING'                                 
 FILLER02 W-RUBRIK   +18  1 A VALUE ';'                                         
 RUB-3    W-RUBRIK   +19  2 A VALUE 'LS'                                        
 FILLER03 W-RUBRIK   +21  1 A VALUE ';'                                         
 RUB-4    W-RUBRIK   +22  2 A VALUE 'L0'                                        
 FILLER04 W-RUBRIK   +24  1 A VALUE ';'                                         
 RUB-5    W-RUBRIK   +25  4 A VALUE 'GÅNG'                                      
 FILLER05 W-RUBRIK   +29  1 A VALUE ';'                                         
 RUB-6    W-RUBRIK   +30  5 A VALUE 'PLATS'                                     
 FILLER06 W-RUBRIK   +35  1 A VALUE ';'                                         
 RUB-7    W-RUBRIK   +36 19 A VALUE 'HÅLLBARHET I VECKOR'                       
 FILLER07 W-RUBRIK   +55  1 A VALUE ';'                                         
 *RUB-8    W-RUBRIK   +56 11 A VALUE 'INLEV DATUM'                              
 *FILLER08 W-RUBRIK   +67  1 A VALUE ';'                                        
                                                                                
 W-UT-RAD                       W   80 A                                        
 W-UT-IDARTNR       W-UT-RAD     +1  9 N                                        
 W-UT-FILLER01      W-UT-RAD    +10  1 A VALUE ';'                              
 W-UT-BEART-SVE     W-UT-RAD    +11 25 A                                        
 W-UT-FILLER02      W-UT-RAD    +36  1 A VALUE ';'                              
 W-UT-KVLS          W-UT-RAD    +37  8 N                                        
 W-UT-FILLER03      W-UT-RAD    +45  1 A VALUE ';'                              
 W-UT-ADLAGOMR      W-UT-RAD    +46  4 N                                        
 W-UT-FILLER04      W-UT-RAD    +50  1 A VALUE ';'                              
 W-UT-ADGANG        W-UT-RAD    +51  5 N                                        
 W-UT-FILLER05      W-UT-RAD    +56  1 A VALUE ';'                              
 W-UT-ADPLATS       W-UT-RAD    +57  5 N                                        
 W-UT-FILLER06      W-UT-RAD    +62  1 A VALUE ';'                              
 W-UT-KEMI-KDFGPRIO W-UT-RAD    +63  5 N                                        
 *W-UT-FILLER07      W-UT-RAD    +68  1 A VALUE ';'                             
 *W-UT-AAMMDD        W-UT-RAD    +69  6 N                                       
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
*                                                                               
JOB INPUT (INFIL01 KEY(KEMI-IDARTNR) INFIL02 KEY(IDARTNR)) +                    
   START A-INIT                                                                 
*                                                                               
   IF MATCHED                                                                   
     IF KEMI-KDFGPRIO > 0 AND KVLS > 0                                          
      IF KDERS < 20 AND KDERS-UTG = 0                                           
***  GÖR OM TILL ARBETSDAGAR ISTÄLLET FÖR VECKOR                                
***  FÅR INTE VARA MER ÄN 999 DAGAR, SUBMODULEN TAR HÖGST 999 DAGAR             
                                                                                
        IF KEMI-KDFGPRIO > 199                                                  
          W-KEMI-KDFGPRIO EQ 999                                                
        ELSE                                                                    
          W-KEMI-KDFGPRIO EQ KEMI-KDFGPRIO * 5                                  
        END-IF                                                                  
***  GRÄNS = 80 VECKOR => 400 ARBETSDAGAR                                       
                                                                                
        IF W-KEMI-KDFGPRIO GT 400                                               
          W-FIFO  =  W-KEMI-KDFGPRIO * 0.90                                     
        ELSE                                                                    
          IF W-KEMI-KDFGPRIO LT 400                                             
            W-FIFO  =  W-KEMI-KDFGPRIO * 0.50                                   
          END-IF                                                                
        END-IF                                                                  
                                                                                
        PERFORM B-DATUM                                                         
                                                                                
        IF W-AAVV-FOM > W-INLEV-AAVV                                            
          W-UT-IDARTNR        EQ IDARTNR                                        
          W-UT-BEART-SVE      EQ BEART-SVE                                      
          W-UT-KVLS           EQ KVLS                                           
          W-UT-ADLAGOMR       EQ ADLAGOMR                                       
          W-UT-ADGANG         EQ ADGANG                                         
          W-UT-ADPLATS        EQ ADPLATS                                        
          W-UT-KEMI-KDFGPRIO  EQ KEMI-KDFGPRIO                                  
*         W-UT-AAMMDD         EQ TIAVIDAT-SEN                                   
          UTPOST EQ W-UT-RAD                                                    
          PUT UTFIL01                                                           
        END-IF                                                                  
      END-IF                                                                    
     END-IF                                                                     
   END-IF                                                                       
A-INIT. PROC                                                                    
                                                                                
  CALL DATKORT  USING (PROGRAM-NAMN +                                           
                       DATUMKORT-ID +                                           
                       DATKORT-DATUMKORT)                                       
                                                                                
  DAGENS-AAR       =  DATKORT-D-AAR                                             
  W-DAGENS-AA      =  DATKORT-D-AAR                                             
  W-DAGENS-AAMM    =  DATKORT-D-MAANAD                                          
  W-DAGENS-AAMMDD  =  DATKORT-D-DAG                                             
  DAGENS-VECKA     =  DATKORT-D-VECKA                                           
  DISPLAY 'DAGENS-DATUM ' W-DAGENS-DATUM                                        
  UTPOST EQ W-RUBRIK                                                            
  PUT UTFIL01                                                                   
END-PROC                                                                        
                                                                                
B-DATUM. PROC                                                                   
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
*** HÄMTAR AAVV FÖR TIAAMMDD-FOM                                                
  DAT-I-TIDATUM = W-DATUM-FOM                                                   
  DAT-KDDATFORM = 'AAMMDD'                                                      
  CALL WDATKONV USING (DAT-KDDATFORM, +                                         
                       DAT-I-TIDATUM, +                                         
                       DAT-O-TIDATUM, +                                         
                       DAT-KDSVAR)                                              
  W-AAVVD-FOM = DAT-TIAAVVD                                                     
                                                                                
*** HÄMTAR AAVV FÖR INLEVERNS DATUM ***                                         
  DAT-I-TIDATUM = TIAVIDAT-SEN                                                  
  DAT-KDDATFORM = 'AAMMDD'                                                      
  CALL WDATKONV USING (DAT-KDDATFORM, +                                         
                       DAT-I-TIDATUM, +                                         
                       DAT-O-TIDATUM, +                                         
                       DAT-KDSVAR)                                              
  W-INLEV-AAVVD  = DAT-TIAAVVD                                                  
END-PROC                                                                        
*                                                                               
//INFIL01  DD  DSN=W111.W100V1.W11184(+0),DISP=SHR                              
//INFIL02  DD  DSN=WXTR.PARTINFO.WCDCWEEK(+0),DISP=SHR                          
//UTFIL01  DD  DSN=&&WCDCWEEK,DISP=(NEW,PASS,DELETE),                           
//            DATACLAS=PSEN,MGMTCLAS=BACKUPC                                    
//OSDAT    DD  DSN=&INDRTE..DATUM(+0),DISP=SHR              -- DATKORT          
//DATIN    DD  DSN=F1ST00.CONSTANT(NORMAL),DISP=SHR         -- DATKORT          
//DATUT    DD  SYSOUT=*                                     -- DATKORT          
//*                                                                             
//* CONVERT FB FILE TO VB FILE                                                  
//SORT     EXEC PGM=SORT                                                        
//SYSOUT   DD  SYSOUT=*                                                         
//SORTIN   DD  DSN=&&WCDCWEEK,DISP=(OLD,DELETE,DELETE)                          
//VBOUT    DD  DSN=WXTR.KEMIREV1.WCDCWEEK(+1),                                  
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEB,MGMTCLAS=NOBACKUP                                  
//SYSIN    DD  *                                                                
 OPTION COPY                                                                    
 OUTFIL FNAMES=VBOUT,FTOV                                                       
//*                                                                             
//* SEND THE LIST VIA EMAIL FROM D&P                                            
//EMPTY1 EXEC WEMPTST,DSIN=WXTR.KEMIREV1.WCDCWEEK(+1)                           
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14DAP2,DSIN=WXTR.KEMIREV1.WCDCWEEK(+1)                                
//SYSIN           DD *                                                          
WCDCWEEK                                                                        
WCDCWEEK                                                                        
//    ENDIF                                                                     
//*                                                                             
