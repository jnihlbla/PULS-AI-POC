//W611S1RE JOB (640W6110100W611S1RE,W100),'RTN W611S1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTZ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W611.W611S1.W61101F(+0)                              
//*                                                                             
//    IF (EMPTY1.T.RC = 0) THEN                                                 
//FIX   EXEC  PGM=V164D0                                                        
//SYSPRINT DD SYSOUT=*                                                          
//SYSIN DD DUMMY,DCB=BLKSIZE=100                                                
//IN DD DSN=W611.W611S1.W61101(+0),DISP=SHR                                     
//UT DD DSN=W611.W611S1.FELFIL(+1),DISP=(NEW,CATLG,DELETE),                     
//      DCB=(RECFM=VB,LRECL=1023,BLKSIZE=27243),                                
//      DATACLAS=PSEN,                                                          
//      MGMTCLAS=NOBACKUP                                                       
//*                                                                             
//      EXEC WZ14PDAP,DSIN=W611.W611S1.FELFIL(+1)                               
//SYSIN           DD *                                                          
W61101-001                                                                      
W61101                                                                          
//    ENDIF                                                                     
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611S1RE                                         
