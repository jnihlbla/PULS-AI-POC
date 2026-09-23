//W611J0E1 JOB (640W6110100W611J0E1,W100),'RTN W611E1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WQREC   EXEC WZ11P013                                                         
//WZ1113.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.VEDIASND                                               
¤DSOUT W611.W611X1SE.W61101                                                     
¤RECFM FB                                                                       
¤LRECL 1009                                                                     
//*                                                                             
//ORDER   EXEC WSOP                                                             
  ORDER W611S1                                                                  
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J0E1                                         
