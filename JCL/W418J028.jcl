//W418J028 JOB (670W4180100W418J028,W100),'RTN W418B2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC JCLLIB ORDER=(W.QASE.PROCLIB)                                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST1                                                     
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//*                                                                             
//* Job started by &IDANSTNR                                                    
//*                     in WEB-appl W407   (Buy back)                           
//*                                                                             
//*                                                                             
//* - - - - - - - - - - Get File From Server - - - - - - -                      
//UNIX    EXEC W001HFSG,                                                        
//             PATHIN='/app/vccs/qase/w407/data/&IDDISTR.w41828.csv',           
//             LRECL=16,RECFM=FB,                                               
//             DSOUT=W418.W418B2.CSV.W41828(+1)                                 
//*                                                                             
//* - - - - - - - - - - - - Run preparation                                     
//*                         This PROC only works together with                  
//*IDANSTNR=&IDANSTNR. IDDISTR=&IDDISTR. IDKUND=&IDKUNDNR.                      
//*SUMINVD=&SUMINVD. REFOBNET=&REFOBNET.                                        
//W418    EXEC W418P028                                                         
//*                                                                             
//W41828.SYSINPUT DD *                                                          
&IDANSTNR,&IDDISTR,&IDKUNDNR,&SUMINVD,&REFOBNET,                                
//*                                                                             
//IFERR  IF W418.W41828.RC>6 THEN                                               
// EXEC WZ14PDAP,DSIN=W418.W418B2.FELMEDL(+0)                                   
//SYSIN           DD *                                                          
BUYBACK-FEL                                                                     
&IDANSTNR                                                                       
//*                                                                             
// ELSE                                                                         
//EMPTY1 EXEC WEMPTST,DSIN=W418.W418B2.W41828(+1)                               
//    IF (EMPTY1.T.RC = 0) THEN                                                 
// EXEC WZ14PDAP,DSIN=W418.W418B2.OKMEDL(+0)                                    
//SYSIN           DD *                                                          
BUYBACK-OK                                                                      
&IDANSTNR                                                                       
//* om programkörning ok och fil med innehåll                                   
// EXEC WSOP                                                                    
   ORDER W418B3                                                                 
//    ELSE                                                                      
//*   tom fil, skicka felmed                                                    
// EXEC WZ14PDAP,DSIN=W418.W418B2.FELMEDL(-1)                                   
//SYSIN           DD *                                                          
BUYBACK-FEL                                                                     
&IDANSTNR                                                                       
//*                                                                             
//    ENDIF                                                                     
//IFERREND  ENDIF                                                               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J028                                         
