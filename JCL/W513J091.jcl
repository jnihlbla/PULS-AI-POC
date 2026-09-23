//W513J091 JOB (670W5130100W513J091,W100),'RTN W513B2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//*  SOP-PARM IDDC: &IDDC                                                       
//*                                                                             
//W513    EXEC W513P091                                                         
//W51391.SYSIN    DD *                                                          
&IDDC                                                                           
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W513J091                                         
